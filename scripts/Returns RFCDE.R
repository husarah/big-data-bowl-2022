###
# Implementing RFCDE for Return End Yard Line
###

# Install/load necessary packages
library(tidyverse)
# devtools::install_github("tpospisi/RFCDE/r")
# to get the above line to work, must install gcc through homebrew and add a line to Makevars file
library(RFCDE)

# Training data set
df <- read_csv("df_w_voronoi.csv")[,-1]
# Read in file for EP grid for mapping to yardline #

returns <- df %>%
  filter(specialTeamsResult == "Return")
non_returns <- df %>%
  filter(specialTeamsResult != "Return")

# Randomize order of data so we can do 10-fold CV
set.seed(2022)
index <- sample(nrow(returns))
returns <- returns[index,]

# Select x variables
x_train <- returns %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining)
z_train <- returns %>%
  select(endYardLine)

x_test <- non_returns %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining) %>%
  data.matrix()
z_test <- non_returns %>%
  select(endYardLine) %>%
  data.matrix()

# Create grid for predictions
z_grid <- 0:100

# K-fold CV
k <- 10
folds <- c(rep(1:k, each = nrow(x_train)/10), 1:8)
returns$fold <- folds
x_train$fold <- folds
z_train$fold <- folds
cv_preds <- NULL
for(i in 1:k){
  train_x <- x_train %>%
    filter(fold != i) %>%
    select(-fold) %>%
    data.matrix()
  test_x <- x_train %>%
    filter(fold == i) %>%
    select(-fold) %>%
    data.matrix()
  train_z <- z_train %>%
    filter(fold != i) %>%
    select(-fold) %>%
    data.matrix()
  set.seed(2022)
  model <- RFCDE(train_x, train_z, n_trees = 10000)
  test_preds <- predict(model, test_x, "CDE", z_grid)
  test_preds <- test_preds/rowSums(test_preds)
  cv_preds <- rbind(cv_preds, test_preds)
}

# Prep matrices for full data
x_train <- returns %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining)
z_train <- returns %>%
  select(endYardLine)

x_train <- data.matrix(x_train)
z_train <- data.matrix(z_train)

# Run RFCDE model
set.seed(2022)
return_model <- RFCDE(x_train, z_train, n_trees = 10000)

# Assign predictions
preds <- predict(return_model, x_test, "CDE", z_grid)

# Rescale densities to sum to 1 (make them probabilities)
preds <- preds/rowSums(preds)

# Combine CV & test preds into one data set
all_preds <- rbind(cv_preds, preds)

# Multiply each probability by EP and take mean for each prediction to get E[EP]
ep_grid <- matrix(ep_grid, ncol = length(ep_grid), nrow = nrow(all_preds), byrow =T)
all_preds_ep <- rowSums(all_preds*ep_grid)
all_plays <- bind_rows(returns, non_returns)
all_plays$pred_ep_return <- all_preds_ep

# write_csv(all_plays, "all_plays_return.csv")
