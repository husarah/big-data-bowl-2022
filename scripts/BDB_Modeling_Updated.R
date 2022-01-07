setwd("/Users/zbradlow/Desktop")
# load libraries
library(tidyverse)
library(caret)
library(gganimate)
library(gifski)
library(av)
library(BB)
library(gt)
library(Hmisc)
library(pracma)
library(glmnetUtils)
library(rpart)
library(mvtnorm)
library(rpart.plot)
library(randomForest)
library(foreach)
library(nflfastR)
df_with_voronoi <- read_csv("df_w_voronoi.csv")
ep_grid <- read_csv("ep_grid_2.csv")
ep_grid_yards <- read_csv("ep_grid.csv")[-102,]

df_with_voronoi = df_with_voronoi %>%
  mutate(muffed_punt = as.factor(muffed_punt)) %>%
  mutate(fumble = as.factor(fumble))

final_df <- df_with_voronoi %>%
  filter(!specialTeamsResult == 'Out of Bounds') %>%
  filter(ballFinalX >= 10, ballFinalX <= 110, ballFinalY >= 0, ballFinalY <= 160/3) 
  
table(df_with_voronoi$specialTeamsResult)
table(final_df$specialTeamsResult)

# write.csv(final_df, "final_df.csv")

train <- final_df %>% 
  filter(specialTeamsResult == 'Fair Catch' | specialTeamsResult == 'Return' | specialTeamsResult == 'Muffed') %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, muffed_punt)

test <- final_df %>% 
  filter(specialTeamsResult == 'Out of Bounds' | specialTeamsResult == 'Downed' | specialTeamsResult == 'Touchback') %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, muffed_punt)

k <- 10
folds <- c(rep(1:k, each = nrow(train)/10), 1:6)
train <- train %>%
  mutate(fold = folds)
cv_preds <- NULL
for(i in 1:k){
  train_df <- train %>%
    filter(fold != i) %>%
    select(-fold)
  test_df <- train %>%
    filter(fold == i) %>%
    select(-fold)
  set.seed(2022)
  model <- randomForest(muffed_punt ~ ., data = train_df, importance = TRUE)
  test_preds <- predict(model, test_df, type = "prob")
  cv_preds <- rbind(cv_preds, test_preds)
}

muff_model <- randomForest(muffed_punt ~ ., data = train %>% select(-fold), importance = TRUE)

# Assign predictions
preds <- predict(muff_model, test, type = "prob")

# Combine CV & test preds into one data set
all_preds <- rbind(cv_preds, preds)

all_plays <- final_df %>%
  mutate(pred_muff = all_preds[,2],
         yardLine = round(ballFinalX),
         oppyardLine = 120 - yardLine) %>%
  left_join(ep_grid) %>%
  select(-yardLine) %>%
  rename("teamEP" = "EP", "yardLine" = "oppyardLine") %>%
  left_join(ep_grid) %>%
  select(-yardLine) %>%
  rename("oppEP" = "EP") %>%
  mutate(oppEP = oppEP * -1,
         fair_catch_ep = (1 - (1 - 0.6776) * pred_muff) * teamEP + 
           (1 - 0.6776) * pred_muff * oppEP)

train <- final_df %>% 
  filter(specialTeamsResult == 'Out of Bounds' | specialTeamsResult == 'Downed' | specialTeamsResult == 'Touchback') %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, last_x_frame)

test <- final_df %>% 
  filter(specialTeamsResult == 'Fair Catch' | specialTeamsResult == 'Return' | specialTeamsResult == 'Muffed') %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, last_x_frame)

k <- 10
folds <- c(rep(1:k, each = nrow(train)/10), 1:3)
train <- train %>%
  mutate(fold = folds)
cv_preds <- data.frame()
for(i in 1:k){
  train_df <- train %>%
    filter(fold != i) %>%
    select(-fold)
  test_df <- train %>%
    filter(fold == i) %>%
    select(-fold)
  set.seed(2022)
  model <- randomForest(last_x_frame ~ ., data = train_df, importance = TRUE)
  test_preds <- data.frame(predict(model, test_df, type = "response")) 
  cv_preds <- rbind(cv_preds, test_preds)
}

bounce_model <- randomForest(last_x_frame ~ ., data = train %>% select(-fold), importance = TRUE)

# Assign predictions
preds <- data.frame(predict(bounce_model, test, type = "response"))
colnames(cv_preds) = "value"
colnames(preds) = "value"

# Combine CV & test preds into one data set
all_preds <- rbind(cv_preds, preds)
all_plays_bounce<- read_csv("all_plays_bounce.csv") %>%
  select(gameId, playId, pred_ep_bounce)

bounce_df <- final_df %>%
  right_join(all_plays_bounce, by = c("gameId", "playId")) %>%
  rename("let_it_go_ep" = "pred_ep_bounce") %>%
  select(gameId, playId, let_it_go_ep)


# bounce_df <- final_df %>%
#   mutate(pred_bounce = as.double(all_preds[,1]),
#          yardLine = round(pred_bounce)) %>%
#   left_join(ep_grid) %>%
#   select(-yardLine) %>%
#   rename("bounce_ep" = "EP") %>%
#   select(gameId, playId, bounce_ep)

all_plays <- all_plays %>%
  left_join(bounce_df, by = c("gameId", "playId"))

returns_train <- all_plays %>%
  filter(specialTeamsResult == "Return") %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, fumble)

returns_test <- all_plays %>%
  filter(!specialTeamsResult == "Return") %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, fumble)

k <- 10
folds <- c(rep(1:k, each = nrow(returns_train)/10), 1:5)
train <- returns_train %>%
  mutate(fold = folds)
cv_preds <- NULL
for(i in 1:k){
  train_df <- train %>%
    filter(fold != i) %>%
    select(-fold)
  test_df <- train %>%
    filter(fold == i) %>%
    select(-fold)
  set.seed(2022)
  fumble_model <- randomForest(as.factor(fumble) ~ ., data = train_df, importance = TRUE)
  test_preds <- predict(fumble_model, test_df, type = "prob")
  cv_preds <- rbind(cv_preds, test_preds)
}

fumble_model <- randomForest(as.factor(fumble) ~ ., data = train %>% select(-fold), importance = TRUE)

# Assign predictions
preds <- predict(fumble_model, returns_test, type = "prob")

# Combine CV & test preds into one data set
all_preds <- rbind(cv_preds, preds)

all_plays_updated <- all_plays %>%
  mutate(pred_fumble = all_preds[,2]) 

train <- final_df %>% 
  filter(specialTeamsResult == "Return") %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, endYardLine)

test <- final_df %>% 
  filter(!specialTeamsResult == "Return") %>%
  select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
         sep_opp_1:voronoi_area, timeRemaining, endYardLine)

k <- 10
folds <- c(rep(1:k, each = nrow(train)/10), 1:5)
train <- train %>%
  mutate(fold = folds)
cv_preds <- data.frame()
for(i in 1:k){
  train_df <- train %>%
    filter(fold != i) %>%
    select(-fold)
  test_df <- train %>%
    filter(fold == i) %>%
    select(-fold)
  set.seed(2022)
  model <- randomForest(endYardLine ~ ., data = train_df, importance = TRUE)
  test_preds <- data.frame(predict(model, test_df, type = "response")) 
  cv_preds <- rbind(cv_preds, test_preds)
}

return_model <- randomForest(endYardLine ~ ., data = train %>% select(-fold), importance = TRUE)

# Assign predictions
preds <- data.frame(predict(return_model, test, type = "response"))
colnames(cv_preds) = "value"
colnames(preds) = "value"

# Combine CV & test preds into one data set
all_preds <- rbind(cv_preds, preds)

all_plays_return <- read_csv("all_plays_return.csv") %>%
  select(gameId, playId, pred_ep_return)

return_df <- final_df %>%
  right_join(all_plays_return, by = c("gameId", "playId")) %>%
  rename("return_ep" = "pred_ep_return") %>%
  select(gameId, playId, return_ep)

all_plays_updated <- all_plays_updated %>%
  left_join(return_df, by = c("gameId", "playId")) %>%
  mutate(yardLine = 100 - (round(ballFinalX) - 10)) %>%
  left_join(ep_grid) %>%
  select(-yardLine) %>%
  rename("oppEPFumble" = "EP") %>%
  mutate(oppEPFumble = oppEPFumble * -1,
         return_ep = (1 - (1 - 0.674) * pred_fumble) * return_ep + 
           (1 - 0.674) * pred_fumble * oppEPFumble)  %>%
  arrange(desc(fair_catch_ep))

DEEPS <- all_plays_updated %>%
  mutate(decision_ep = case_when(
    specialTeamsResult == "Out of Bounds" ~ let_it_go_ep,
    specialTeamsResult == "Touchback" ~ let_it_go_ep,
    specialTeamsResult == "Downed" ~ let_it_go_ep,
    specialTeamsResult == "Fair Catch" ~ fair_catch_ep,
    specialTeamsResult == "Muffed" ~ fair_catch_ep,
    specialTeamsResult == "Return" ~ return_ep)) %>%
  group_by(gameId, playId) %>%
  mutate(optimal_ep = max(let_it_go_ep, fair_catch_ep, return_ep),
         below_average_ep = decision_ep - optimal_ep,
         correct_decision = decision_ep == optimal_ep) %>%
  ungroup() %>%
  group_by(playerNameRet) %>%
  summarise(deeps_metric = sum(below_average_ep),
            number_punts = n(),
            total_correct_decisions = sum(correct_decision)) %>%
  mutate(deeps_metric_play = deeps_metric / number_punts,
         decision_making_rate = total_correct_decisions / number_punts) %>%
  filter(number_punts >= 14)

varImpPlot(catchable_model)
varImpPlot(fumble_model)
varImpPlot(muff_model)
varImpPlot(bounce_model)
varImpPlot(return_model)

summary(fumble_model)

multiclassifier <- all_plays_updated %>%
  group_by(gameId, playId) %>%
  mutate(optimal_ep = which.max(c(let_it_go_ep, fair_catch_ep, return_ep))) %>%
  ungroup() %>%
  mutate(optimal_ep = as.factor(optimal_ep))
classifier_model <- randomForest(as.factor(optimal_ep) ~ ., data = multiclassifier %>% select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
                                                                         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
                                                                         sep_opp_1:voronoi_area, timeRemaining,optimal_ep,-fold), importance = TRUE)

# Assign predictions
preds <- data.frame(predict(classifier_model, multiclassifier %>% select(frameId, xRet:yardsToGo, pointDiff, operationTime, numGunners:ballApexY,
                                                                         apexTime:ballFinalY, distanceFromBallFinal:distanceFromBallApex, dirChange, 
                                                                         sep_opp_1:voronoi_area, timeRemaining,optimal_ep,-fold), type = "prob"))
colnames(preds) = "value"
all_classifcation_preds = rbind(preds)

classifier_importance <- importance(classifier_model)

summary_stats <- all_plays_updated %>%
  mutate(decision_ep = case_when(
    specialTeamsResult == "Out of Bounds" ~ let_it_go_ep,
    specialTeamsResult == "Touchback" ~ let_it_go_ep,
    specialTeamsResult == "Downed" ~ let_it_go_ep,
    specialTeamsResult == "Fair Catch" ~ fair_catch_ep,
    specialTeamsResult == "Muffed" ~ fair_catch_ep,
    specialTeamsResult == "Return" ~ return_ep)) %>%
  group_by(gameId, playId) %>%
  mutate(optimal_ep = max(let_it_go_ep, fair_catch_ep, return_ep),
         below_average_ep = decision_ep - optimal_ep,
         correct_decision = decision_ep == optimal_ep,
         let_it_go_p = let_it_go_ep == optimal_ep,
         fair_catch_p = fair_catch_ep == optimal_ep,
         return_p = return_ep == optimal_ep) %>%
  ungroup() %>%
  summarise(let_it_go = sum(let_it_go_p),
            fair_catch = sum(fair_catch_p),
            return = sum(return_p))

