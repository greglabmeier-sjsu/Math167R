# MLB PCA lesson code
# Extracted from mlb_pca_lesson.qmd
# Run this script from top to bottom in RStudio.

# ============================================================
# Setup
# ============================================================
library(tidyverse)
library(Lahman) 

# ============================================================
# PCA concepts: what PCA does > A simple two-variable example
# ============================================================
example_data <- tibble(
  power = c(1, 2, 3, 4, 5),
  contact = c(1.2, 2.1, 2.8, 4.2, 5.1)
)

example_pca <- prcomp(
  example_data,
  center = TRUE,
  scale. = TRUE
)

example_pca$rotation
example_pca$x

# The two loading vectors are orthogonal.
sum(example_pca$rotation[, 1] * example_pca$rotation[, 2])

# ============================================================
# 3. Load the baseball data
# ============================================================
# Install once if needed:
# install.packages(c("tidyverse", "Lahman"))

library(tidyverse)
library(Lahman)

glimpse(Batting)

# ============================================================
# 4. Create one row per player-season
# ============================================================
counting_variables <- c(
  "G", "AB", "R", "H", "X2B", "X3B", "HR",
  "RBI", "SB", "CS", "BB", "SO", "IBB",
  "HBP", "SH", "SF", "GIDP"
)

batting_season <- Batting |>
  filter(yearID >= 1900) |>
  group_by(playerID, yearID) |>
  summarize(
    across(
      all_of(counting_variables),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  filter(AB >= 200)

# ============================================================
# 5. Add player position
# ============================================================
player_position <- Fielding |>
  filter(yearID >= 1900) |>
  group_by(playerID, yearID, POS) |>
  summarize(
    fielding_games = sum(G, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(playerID, yearID) |>
  slice_max(fielding_games, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    playerID,
    yearID,
    Position = POS
  )

batting_season <- batting_season |>
  left_join(player_position, by = c("playerID", "yearID")) |>
  mutate(Position = replace_na(Position, "Unknown"))

# ============================================================
# 6. Create plate appearances and batting rates
# ============================================================
batting_season <- batting_season |>
  mutate(
    PA = AB + BB + HBP + SH + SF,

    # Traditional rates
    BA = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = (H + X2B + 2 * X3B + 3 * HR) / AB,

    # Uncomment if you want OPS in the analysis.
    # OPS = OBP + SLG,

    # Event rates per plate appearance
    R_rate = R / PA,
    X2B_rate = X2B / PA,
    X3B_rate = X3B / PA,
    HR_rate = HR / PA,
    RBI_rate = RBI / PA,
    SB_rate = SB / PA,
    BB_rate = BB / PA,
    SO_rate = SO / PA

    # Optional variables. Add a comma after SO_rate if enabling these:
    # ,CS_rate = CS / PA
    # ,IBB_rate = IBB / PA
    # ,HBP_rate = HBP / PA
    # ,SH_rate = SH / PA
    # ,SF_rate = SF / PA
    # ,GIDP_rate = GIDP / PA
  ) |>
  filter(if_all(where(is.numeric), is.finite))

# ============================================================
# 7. Choose the variables for PCA
# ============================================================
rate_variable_names <- c(
  "BA", "OBP", "SLG",
  "R_rate", "X2B_rate", "X3B_rate",
  "HR_rate", "RBI_rate", "SB_rate",
  "BB_rate", "SO_rate"
)

analysis_data <- batting_season |>
  left_join(
    People |>
      transmute(
        playerID,
        player_name = paste(nameFirst, nameLast)
      ),
    by = "playerID"
  ) |>
  select(
    any_of(c("playerID", "player_name", "yearID", "Position")),
    all_of(rate_variable_names)
  )

analysis_data |>
  select(any_of(c("playerID", "player_name", "yearID", "Position"))) |>
  slice_head(n = 10)

# ============================================================
# 8. Run PCA using the original rates
# ============================================================
rate_variables <- analysis_data |>
  select(all_of(rate_variable_names)) |>
  select(where(~ sd(.x, na.rm = TRUE) > 0))

pc_rates <- prcomp(
  rate_variables,
  center = TRUE,
  scale. = TRUE
)

summary(pc_rates)
pc_rates$rotation

# ============================================================
# 8. Run PCA using the original rates
# ============================================================
rate_scores <- as_tibble(pc_rates$x) |>
  bind_cols(
    analysis_data |>
      select(any_of(c("playerID", "player_name", "yearID", "Position")))
  )

rate_scores |>
  select(any_of(c("player_name", "yearID", "Position", "PC1", "PC2"))) |>
  slice_head(n = 10)

ggplot(rate_scores, aes(PC1, PC2)) +
  geom_point(alpha = 0.30) +
  theme_minimal() +
  labs(
    title = "MLB Player-Seasons Using Batting-Rate PCA",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

# ============================================================
# 9. Normalize rates within each baseball year
# ============================================================
within_year_z <- function(x) {
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)

  if (is.na(x_sd) || x_sd == 0) {
    rep(0, length(x))
  } else {
    (x - x_mean) / x_sd
  }
}

z_rate_data <- analysis_data |>
  group_by(yearID) |>
  mutate(
    across(
      all_of(rate_variable_names),
      within_year_z,
      .names = "z_{.col}"
    )
  ) |>
  ungroup()

z_rate_variable_names <- paste0("z_", rate_variable_names)

z_rate_variables <- z_rate_data |>
  select(all_of(z_rate_variable_names)) |>
  select(where(~ sd(.x, na.rm = TRUE) > 0))

pc_z_rates <- prcomp(
  z_rate_variables,
  center = TRUE,
  scale. = FALSE
)

summary(pc_z_rates)
pc_z_rates$rotation

z_rate_scores <- as_tibble(pc_z_rates$x) |>
  bind_cols(
    z_rate_data |>
      select(any_of(c("playerID", "player_name", "yearID", "Position")))
  )

# ============================================================
# 9. Normalize rates within each baseball year
# ============================================================
ggplot(z_rate_scores, aes(PC1, PC2, color = yearID)) +
  geom_point(alpha = 0.40) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Era-Normalized Batting PCA",
    subtitle = "Each rate is standardized relative to its own baseball year",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Year"
  )

# ============================================================
# 10. Add Hall of Fame status
# ============================================================
hof_players <- HallOfFame |>
  filter(inducted == "Y") |>
  distinct(playerID)

z_rate_scores <- z_rate_scores |>
  mutate(
    HOF = if_else(
      playerID %in% hof_players$playerID,
      "Hall of Fame",
      "Not Hall of Fame"
    )
  )

table(z_rate_scores$HOF)

ggplot(z_rate_scores, aes(PC1, PC2, color = HOF)) +
  geom_point(alpha = 0.35) +
  theme_minimal() +
  labs(
    title = "Era-Normalized PCA by Hall of Fame Status",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Status"
  )

# ============================================================
# 10. Add Hall of Fame status > Optional: run a separate PCA using Hall of Fame player-seasons
# ============================================================
hof_z_data <- z_rate_data |>
  filter(playerID %in% hof_players$playerID)

hof_z_variables <- hof_z_data |>
  select(all_of(z_rate_variable_names)) |>
  select(where(~ sd(.x, na.rm = TRUE) > 0))

pc_hof <- prcomp(
  hof_z_variables,
  center = TRUE,
  scale. = FALSE
)

summary(pc_hof)
pc_hof$rotation

hof_scores <- as_tibble(pc_hof$x) |>
  bind_cols(
    hof_z_data |>
      select(any_of(c("playerID", "player_name", "yearID", "Position")))
  )

ggplot(hof_scores, aes(PC1, PC2, color = Position)) +
  geom_point(alpha = 0.45) +
  theme_minimal() +
  labs(
    title = "PCA Using Hall of Fame Player-Seasons Only",
    x = "Hall of Fame PC1",
    y = "Hall of Fame PC2",
    color = "Position"
  )

# ============================================================
# 11. Compare PC1 and PC2 by year and position
# ============================================================
ggplot(z_rate_scores, aes(PC1, PC2, color = yearID)) +
  geom_point(alpha = 0.35) +
  facet_wrap(~ Position) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Era-Normalized PCA by Year and Position",
    x = "PC1",
    y = "PC2",
    color = "Year"
  )

ggplot(z_rate_scores, aes(x = Position, y = PC1, fill = Position)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.15) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "PC1 Distribution by Position",
    x = "Position",
    y = "PC1"
  )

ggplot(z_rate_scores, aes(x = Position, y = PC2, fill = Position)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.15) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "PC2 Distribution by Position",
    x = "Position",
    y = "PC2"
  )

# ============================================================
# 11. Compare PC1 and PC2 by year and position > Position centers in PCA space
# ============================================================
position_centers <- z_rate_scores |>
  group_by(Position) |>
  summarize(
    mean_PC1 = mean(PC1, na.rm = TRUE),
    mean_PC2 = mean(PC2, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(position_centers, aes(mean_PC1, mean_PC2, label = Position)) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_vline(xintercept = 0, color = "gray80") +
  geom_point(size = 3, color = "steelblue") +
  geom_text(nudge_y = 0.12, check_overlap = TRUE) +
  theme_minimal() +
  labs(
    title = "Average PCA Location by Position",
    x = "Average PC1",
    y = "Average PC2"
  )

# ============================================================
# 12. Find extreme and near-zero player-seasons
# ============================================================
get_pc_examples <- function(data, component, n = 20) {
  scores <- data |>
    select(
      any_of(c("playerID", "player_name", "yearID", "Position", "HOF")),
      all_of(component)
    ) |>
    rename(score = all_of(component))

  list(
    highest = scores |>
      arrange(desc(score)) |>
      slice_head(n = n) |>
      mutate(category = "Highest"),

    lowest = scores |>
      arrange(score) |>
      slice_head(n = n) |>
      mutate(category = "Lowest"),

    closest_to_zero = scores |>
      mutate(distance_from_zero = abs(score)) |>
      arrange(distance_from_zero) |>
      slice_head(n = n) |>
      select(-distance_from_zero) |>
      mutate(category = "Closest to zero")
  )
}

pc1_examples <- get_pc_examples(z_rate_scores, "PC1", n = 20)
pc2_examples <- get_pc_examples(z_rate_scores, "PC2", n = 20)

all_pc_examples <- bind_rows(
  pc1_examples$highest |> mutate(component = "PC1"),
  pc1_examples$lowest |> mutate(component = "PC1"),
  pc1_examples$closest_to_zero |> mutate(component = "PC1"),
  pc2_examples$highest |> mutate(component = "PC2"),
  pc2_examples$lowest |> mutate(component = "PC2"),
  pc2_examples$closest_to_zero |> mutate(component = "PC2")
) |>
  select(
    component,
    category,
    any_of(c("player_name", "playerID", "yearID", "Position", "HOF")),
    score
  )

all_pc_examples

# ============================================================
# 12. Find extreme and near-zero player-seasons
# ============================================================
pc1_examples$highest
pc1_examples$lowest
pc1_examples$closest_to_zero

pc2_examples$highest
pc2_examples$lowest
pc2_examples$closest_to_zero

# ============================================================
# Salary and PC1 comparison
# ============================================================
last_season <- min(
  max(batting_season$yearID),
  max(Salaries$yearID)
)

last_season

# ============================================================
# Salary and PC1 comparison
# ============================================================
salary_by_season <- Salaries |>
  filter(yearID == last_season) |>
  group_by(playerID, yearID) |>
  summarize(
    salary = sum(salary, na.rm = TRUE),
    .groups = "drop"
  )

salary_scores <- bind_rows(
  rate_scores |>
    filter(yearID == last_season) |>
    select(playerID, player_name, yearID, Position, PC1) |>
    mutate(model = "Original-rate PCA"),
  z_rate_scores |>
    filter(yearID == last_season) |>
    select(playerID, player_name, yearID, Position, PC1) |>
    mutate(model = "Era-normalized PCA")
) |>
  left_join(salary_by_season, by = c("playerID", "yearID")) |>
  filter(!is.na(salary), salary > 0) |>
  mutate(log_salary = log10(salary))

salary_scores

# ============================================================
# Salary and PC1 comparison
# ============================================================
ggplot(salary_scores, aes(salary, PC1, color = model)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10(labels = scales::label_dollar()) +
  facet_wrap(~ model) +
  theme_minimal() +
  labs(
    title = paste("PC1 and Salary in", last_season),
    subtitle = "Salary is shown on a logarithmic scale",
    x = "Total annual salary",
    y = "PC1",
    color = "PCA model"
  )

# ============================================================
# Salary and PC1 comparison
# ============================================================
salary_correlations <- salary_scores |>
  group_by(model) |>
  summarize(
    n = n(),
    correlation_salary = cor(PC1, salary, use = "complete.obs"),
    correlation_log_salary = cor(PC1, log_salary, use = "complete.obs"),
    .groups = "drop"
  )

salary_correlations

# ============================================================
# Compare top and bottom PC1 player-seasons
# ============================================================
get_pc1_extremes <- function(data, model_name, n = 20) {
  bind_rows(
    data |>
      arrange(desc(PC1)) |>
      slice_head(n = n) |>
      mutate(category = "Top PC1"),
    data |>
      arrange(PC1) |>
      slice_head(n = n) |>
      mutate(category = "Bottom PC1")
  ) |>
    mutate(model = model_name) |>
    select(
      model,
      category,
      any_of(c("player_name", "playerID", "yearID", "Position", "HOF")),
      PC1
    )
}

pc1_original_extremes <- get_pc1_extremes(
  rate_scores,
  "Original-rate PCA"
)

pc1_z_extremes <- get_pc1_extremes(
  z_rate_scores,
  "Era-normalized PCA"
)

pc1_comparison <- bind_rows(
  pc1_original_extremes,
  pc1_z_extremes
)

pc1_comparison

# ============================================================
# Compare top and bottom PC1 player-seasons
# ============================================================
pc1_comparison |>
  group_by(model, category) |>
  slice_head(n = 5) |>
  ungroup()


