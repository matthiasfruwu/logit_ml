
library(tidyverse)
library(corrplot)

# Function to read and prepare movie data
read_and_prepare_movie_data <- function(path) {
  df <- read_csv(path)
  df <- df %>%
    mutate(is_indie = if_else(distri_type == "Independent", 1, 0))
  return(df)
}

# Function to plot distributions
distribution_plots <- function(df_movie) {
  p1 <- ggplot(df_movie, aes(x = factor(is_bo_bomb))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Box Office Bomb", x = "Is Box Office Bomb", y = "Count") +
    theme_minimal()

  p2 <- ggplot(df_movie, aes(x = runtime)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30) +
    labs(title = "Runtime Distribution", x = "Runtime (minutes)", y = "Count") +
    theme_minimal()

  p3 <- ggplot(df_movie, aes(x = critic_score)) +
    geom_histogram(aes(y = ..density..), fill = "steelblue", color = "white", bins = 30, alpha = 0.5) +
    geom_density(color = "red", size = 1) +
    labs(title = "Critic Score Distribution", x = "Critic Score", y = "Density") +
    theme_minimal()

  p4 <- ggplot(df_movie, aes(x = factor(is_indie))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Indie Movies", x = "Is Indie", y = "Count") +
    theme_minimal()

  (p1 + p2) / (p3 + p4)
}

# Function to calculate and plot correlation matrix
correlation_matrix <- function(df_movie) {
  numeric_vars <- df_movie %>% select(runtime, imdb_avg_rating, audience_score, critic_score, is_indie)
  corr_mat <- cor(numeric_vars, use = "complete.obs")
  corrplot(corr_mat, method = "number", type = "upper", tl.cex = 0.8)
}
