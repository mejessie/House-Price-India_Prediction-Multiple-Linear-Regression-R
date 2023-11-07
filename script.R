# This code has been written by Jedsada Aimjit.
library("tidyverse")
library("caret")
library("readxl")

# Split data function
split_df <- function(df, per) {
  n <- nrow(df)
  id <- sample(1:n, size = per * n)
  train_set <- df[id, ]
  test_set <- df[-id, ]
  return(list(train_set, test_set))
}

# Import data
df <- read_excel("House Price India.xlsx")

# Preview df
View(df)

# Prep data
df %>%
  complete.cases() %>%
  mean()
# It returns 1 that means it's clear

# Renaming columns
# replace " " with "_"
names(df) <- gsub(" ", "_", names(df))

# Select all except id, Date
df <- df %>%
  select(-id, -Date)

# Split data
splited_df <- split_df(df, 0.80)
train_set <- splited_df[[1]]
test_set <- splited_df[[2]]

# Train model
model <- train(Price ~ ., data = train_set, method = "lm")
# print(model)
# varImp(model)

# Prediction
y_hat <- predict(model, newdata = test_set)

# Evaluate model
# RMSE
sqrt(mean((test_set$Price - y_hat) ** 2))

# MAE
mean(abs(test_set$Price - y_hat))