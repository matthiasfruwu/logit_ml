library(tidyverse)
options(scipen=999)

## vectorized function
clean_money_string <- function(money_string) {
  money_string %>%
    str_remove_all("[$,]") %>%
    na_if("N/A") %>%
    as.numeric() %>%
    return()
}

## non-vectorized function for converting running time
## string to minutes
convert_runtime <- function(rtime) {
  # split the string into pieces, e.g. "1 hr 35 min" 
  # "1" "hr" "35" "min"
  if (is.na(rtime)) {return(NA_integer_)}
  
  parts <- str_split(rtime, " ", simplify = TRUE)
  
  if (length(parts) == 4) {
    # e.g., "1 hr 35 min"
    hours <- as.integer(parts[1])
    minutes <- as.integer(parts[3])
  } else if (length(parts) == 2 && parts[2] == "hr") {
    # e.g., "2 hr"
    hours <- as.integer(parts[1])
    minutes = 0
  } else if (length(parts) == 2 && parts[2] == "min") {
    # e.g., "45 min"
    hours = 0
    minutes <- as.integer(parts[1])
  } else return(NA_integer_)
  
  return(60 * hours + minutes)
}

df_movie <- read_csv("raw_movie_data.csv") 

df_movie <- df_movie %>%
  janitor::clean_names()

df_movie <- df_movie %>%
  mutate(movie_id = str_extract(url, "tt\\d+"),
         domestic_distributor = str_trim(str_remove_all(domestic_distributor, "See full company information$")),
         title_year = str_sub(title, -5, -2),
         release_date = as.Date(str_remove(earliest_release_date, "\n.*"), format = "%B %d, %Y"),
         genres = str_squish(genres)) %>%
  mutate(across(c(domestic, international, worldwide, budget, domestic_opening), clean_money_string)) %>%
  rowwise() %>%
  mutate(runtime = convert_runtime(running_time)) %>%
  ungroup() %>%
  filter(str_sub(title_year, 1, 1) %in% c(1,2))

df_movie <- df_movie %>%
  mutate(mpaa = case_when(
    mpaa %in% c("G", "PG") ~ "PG",
    mpaa == "PG-13"        ~ "PG-13",
    mpaa %in% c("R", "NC-17") ~ "R"
  ))

studios <- read_csv("Classified_Studios.csv") %>% select(c(1,3)) %>% rename(domestic_distributor = Var1, distri_type = Classification) %>% 
  distinct(domestic_distributor, .keep_all = T) %>% mutate(domestic_distributor = str_squish(domestic_distributor))

df_movie <- df_movie %>% mutate(domestic_distributor = str_squish(domestic_distributor)) %>%
  left_join(studios, by = "domestic_distributor")
df_movie <- df_movie %>% 
  mutate(distri_type = replace_na(distri_type, "Independent"),
         distri_type = recode(distri_type, "Unknown" = "Independent"))

imdb_raw <- read_csv("imdb_raw.csv")

df_movie <- df_movie %>% 
  select(-c(budget)) %>%
  left_join(imdb_raw %>% select(movie_id, budget), by = "movie_id")

df_ratings <- read_tsv("title.ratings.tsv.gz") %>% rename(imdb_avg_rating = averageRating, imdb_numvotes = numVotes)
df_movie <- df_movie %>%
  inner_join(df_ratings, by = c("movie_id"="tconst"))

df_movie <- df_movie %>%
  filter(!is.na(budget) & !is.na(worldwide)) %>%
  filter(title_year >= 1995 & budget >= 25e+6 & !(title_year %in% c(2020, 2021, 2022, 2025))) %>%
  mutate(ROI_world = pmin(worldwide/budget,10),
         ROI_domestic = pmin(domestic/budget,5))

df_movie %>%
  ggplot(aes(x = ROI_domestic)) +
  geom_histogram(bins = 70, fill = "#69b3a2", color = "white", alpha = 0.9) +
  geom_vline(xintercept = c(0.66), color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  labs(
    title = "Distribution of Domestic ROI",
    x = "Domestic ROI",
    y = "Number of Movies"
  ) +
  theme_minimal(base_size = 14)

df_movie %>%
  ggplot(aes(x = ROI_world)) +
  geom_histogram(bins = 70, fill = "#69b3a2", color = "white", alpha = 0.9) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  labs(
    title = "Distribution of Worldwide ROI",
    x = "Worldwide ROI",
    y = "Number of Movies"
  ) +
  theme_minimal(base_size = 14)

df_bombs <- df_movie %>%
  filter(ROI_world < 1 & ROI_domestic < 0.66)

##
rotten_raw <- read_csv("rotten_tom.csv")

df_movie <- df_movie %>%
  mutate(title_match = title %>%
           str_remove_all("\\[.*?\\]") %>%
           str_remove_all("\\(.*?\\)") %>%
           str_replace_all("[^a-zA-Z0-9 ]", "") %>%
           str_squish() %>%
           str_to_lower()
  ) %>% distinct(title_match, .keep_all = T)

rotten_raw <- rotten_raw %>%
  mutate(title_match = title %>%
           str_remove_all("\\[.*?\\]") %>%
           str_remove_all("\\(.*?\\)") %>%
           str_replace_all("[^a-zA-Z0-9 ]", "") %>%
           str_squish() %>%
           str_to_lower(),
         critic_score = parse_number(critic_score),
         audience_score = parse_number(audience_score)
  ) %>% filter(!is.na(critic_score) & !is.na(audience_score)) %>% distinct(title, .keep_all = T) %>%
  select(title_match, audience_score, critic_score)

df_movie <- df_movie %>% inner_join(rotten_raw, by = "title_match")

df_movie <- df_movie %>% mutate(is_bo_bomb = as.integer(ROI_world < 1 & ROI_domestic <= 0.66))

df_movie %>% write.csv("df_movie_prep.csv")

##
roi_resid <- lm(ROI_domestic ~ factor(title_year), data = df_movie)
df_movie$ROI_domestic_resid <- resid(roi_resid)

roi_residw <- lm(ROI_world ~ factor(title_year), data = df_movie)
df_movie$ROI_world_resid <- resid(roi_residw)

# Then use the residual version in the outcome definition
df_movie <- df_movie %>%
  mutate(is_bo_bomb = as.integer(ROI_world < 1 & ROI_domestic <= 0.66))

df_movie <- df_movie %>%
  mutate(
    ge_action         = as.integer(str_detect(genres, "Action")),
    ge_comedy         = as.integer(str_detect(genres, "Comedy")),
    ge_drama_thriller = as.integer(str_detect(genres, "Drama") | str_detect(genres, "Thriller")),
    ge_scifi_ftsy     = as.integer(str_detect(genres, "Sci-Fi") | str_detect(genres, "Fantasy")),
    ge_crime_mist     = as.integer(str_detect(genres, "Crime") | str_detect(genres, "Mystery")),
    ge_romance        = as.integer(str_detect(genres, "Romance")),
    ge_horror        = as.integer(str_detect(genres, "Horror")),
    ge_animation     = as.integer(str_detect(genres, "Animation"))
  )

df_movie %>% 
  select(-c(1)) %>%
  mutate(across(
    c(domestic, international, worldwide, budget, domestic_opening),
    ~ as.integer(.x / 1e+6)
  )) %>%
  select(title, domestic, budget, domestic_distributor, distri_type, movie_id, title_year, runtime, 
         imdb_avg_rating, imdb_numvotes, ROI_domestic, audience_score, critic_score, starts_with("ge_"), is_bo_bomb) %>% 
  write.csv("movie_select.csv", row.names = F)
