###
# Implementing RFCDE for Bouncing Ball End Yard Line (transforming for touchbacks after)
###

# Install/load necessary packages
library(tidyverse)
# devtools::install_github("tpospisi/RFCDE/r")
# to get the above line to work, must install gcc through homebrew and add a line to Makevars file
library(RFCDE)

# Training data set
df <- read_csv("df_w_voronoi.csv")[,-1]
# Read in file for EP grid for mapping to yardline #

bounces <- df %>%
  filter(specialTeamsResult %in% c("Downed", "Out of Bounds", "Touchback"))
non_bounces <- df %>%
  filter(!(specialTeamsResult %in% c("Downed", "Out of Bounds", "Touchback")))

# Randomize order of data so we can do 10-fold CV
set.seed(2022)
index <- sample(nrow(bounces))
bounces <- bounces[index,]

# Select x variables
x_train <- bounces %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining)
z_train <- bounces %>%
  select(last_x_frame)

x_test <- non_bounces %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining) %>%
  data.matrix()
z_test <- non_bounces %>%
  select(last_x_frame) %>%
  data.matrix()

# Create grid for predictions
z_grid <- 0:120

# K-fold CV
k <- 10
folds <- c(rep(1:k, each = nrow(x_train)/10), 1:3)
bounces$fold <- folds
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
x_train <- bounces %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, apexTime,
         ballApexX:ballApexY, vxy:ballFinalY, distanceFromBallApex, dirChange, o_opp_1:voronoi_area, timeRemaining)
z_train <- bounces %>%
  select(last_x_frame)

x_train <- data.matrix(x_train)
z_train <- data.matrix(z_train)

# Run RFCDE model
set.seed(2022)
bounce_model <- RFCDE(x_train, z_train, n_trees = 10000) # had to change n_basis to 10 so CV would work; can try others, 15 had an issue though

# Assign predictions
preds <- predict(bounce_model, x_test, "CDE", z_grid)

# Rescale densities to sum to 1 (make them probabilities)
preds <- preds/rowSums(preds)

# Combine CV & test preds into one data set
all_preds <- as.matrix(rbind(cv_preds, preds))

# Multiply each probability by EP and take mean for each prediction to get E[EP]
ep_grid_2 <- c(rep(2,11), ep_grid[c(-1, -101)], rep(1.15553044, 11))
ep_grid_2 <- matrix(ep_grid_2, ncol = length(ep_grid_2), nrow = nrow(all_preds), byrow = T)
all_preds_ep <- rowSums(all_preds*ep_grid_2)
all_plays <- bind_rows(bounces, non_bounces)
all_plays$pred_ep_bounce <- all_preds_ep

# write_csv(all_plays, "all_plays_bounce.csv")
