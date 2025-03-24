library(tidyverse)
library(broom)
library(yardstick)
library(margins)

#data("GermanCredit")

df_movie <- read_csv("movie_select.csv") 

df_movie <- df_movie %>%
  mutate(is_indie = if_else(distri_type == "Independent", 1, 0))

logit_model <- glm(is_bo_bomb ~  runtime + imdb_avg_rating + 
                     audience_score + critic_score + is_indie + 
                     ge_action + ge_animation, #+ factor(title_year),
                   data = df_movie,
                   family = binomial)

summary(logit_model)

## Model Fit (Pseudo Rsquared)
pscl::pR2(logit_model)

## Classify new data
new_film <- data.frame(
  runtime = 200,  
  imdb_avg_rating = 6.2,
  audience_score = 33, 
  critic_score = 45,    
  is_indie = 1,
  ge_action = 1,
  ge_animation = 1
)

##
predicted_prob <- predict(logit_model, newdata = new_film, type = "response")

## margins
marginal_effects <- margins(logit_model)
summary(marginal_effects)

# Get predicted probability for the new observation
predicted_prob <- predict(logit_model, newdata = new_film, type = "response")

## Use as a classifier (threshold >= 0.5)
df_movie <- df_movie %>%
  mutate(
    predicted_prob = predict(logit_model, type = "response"),
    predicted_class = if_else(predicted_prob >= 0.5, 1, 0)
  )

df_movie <- df_movie %>%
  mutate(
    truth = factor(is_bo_bomb, levels = c(1, 0), labels = c("Yes", "No")),
    predicted = factor(predicted_class, levels = c(1, 0), labels = c("Yes", "No"))
  )

## Confusion Matrix
##

cm <- conf_mat(df_movie, truth, predicted)
print(cm)

## Metrics
accuracy(df_movie, truth, predicted)
sens(df_movie, truth, predicted)
spec(df_movie, truth, predicted)
precision(df_movie, truth, predicted)



##### XGBOOST #####
library(tidyverse)
library(rsample)
library(xgboost)
library(yardstick)

# Split data into training (70%) and testing (30%)
set.seed(144) # for reproducibility
split <- initial_split(df_movie, prop = 0.7, strata = is_bo_bomb)
train_data <- training(split)
test_data  <- testing(split)

# Define features
features <- c("runtime", "imdb_avg_rating", "audience_score",
              "critic_score", "is_indie", "ge_action", "ge_animation", "ge_drama_thriller",
              "ge_scifi_ftsy", "ge_crime_mist", "ge_horror", "ge_romance", "title_year")

# Prepare training matrices
X_train <- train_data %>% select(all_of(features)) %>% as.matrix()
y_train <- train_data$is_bo_bomb

X_test <- test_data %>% select(all_of(features)) %>% as.matrix()
y_test <- test_data$is_bo_bomb

# Train the XGBoost model
xgb_model <- xgboost(data = X_train, label = y_train,
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     nrounds = 3000, verbose = 0)

# Predict on test data
test_data$predicted_prob <- predict(xgb_model, X_test)
test_data <- test_data %>%
  mutate(predicted_class = if_else(predicted_prob >= 0.5, 1, 0),
         truth = factor(is_bo_bomb, levels = c(1,0), labels = c("Yes", "No")),
         predicted = factor(predicted_class, levels = c(1,0), labels = c("Yes", "No")))

# Evaluate performance on test data
cm <- conf_mat(test_data, truth, predicted)
print(cm)

accuracy(test_data, truth, predicted)
sens(test_data, truth, predicted)
spec(test_data, truth, predicted)
precision(test_data, truth, predicted)
