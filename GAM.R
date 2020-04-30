# Load Libraries ----------------------------------------------------------

library(mgcv)
library(magrittr)
library(gratia)
library(tidyverse)


# Partition Data ----------------------------------------------------------
model_vars <- cpoe_passes %>% 
  select(complete_pass, air_yards, yardline_100, ydstogo, receiver_position, 
         down, qtr, wp, ep, air_is_zero, pass_is_middle, under_two_min, 
         tipped, hail_mary, roof, posteam_type, week, season, qb_hit, shotgun, no_huddle, goal_to_go)

set.seed(69)
train_index_gam <- createDataPartition(y = model_vars$complete_pass, p = .7, list = FALSE)
train_gam <- model_vars[train_index_gam,]
test_gam <- model_vars[-train_index,]



# Train Model -----------------------------------------------------------------
# Try different variables and interactions
gam_model <- train_gam %>%
  bam(
    data = .,
    formula = complete_pass ~
      s(air_yards, k = 12) + s(ydstogo, k = 12) + s(yardline_100, k = 12) + s(wp) + s(ep) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100) + ti(ydstogo, yardline_100) +
      ti(air_yards, ydstogo, by = factor(down)) + ti(air_yards, yardline_100, by = factor(down)) +
      ti(ydstogo, yardline_100, by = factor(down)) + factor(down) +
      factor(receiver_position) + factor(pass_is_middle) +
      factor(tipped) + factor(hail_mary) + factor(roof) + factor(posteam_type) + week + season + 
      factor(qb_hit) + factor(shotgun),
    family = binomial()
  )

#  ** Summary

summary(gam_model)

appraise(gam_model, type = "response")
mgcv::k.check(gam_model)

plot(gam_model, pages = 2)



# Test Model --------- 

gam_pred <- predict.bam(gam_model, newdata = test_gam, type = "response")

gam_pred_data <- data.frame("complete_pass" = test_gam$complete_pass, "pred" = gam_pred)
gam_pred_data %<>% mutate(error = complete_pass - pred)
gam_pred_data %>% summarise(mean_abs_error = mean(abs(error)),
                            mean_sq_error = mean(error^2),
                            root_mean_sq_error = sqrt(mean(error^2)))

# Trained on All Data --------
gam_full <- model_vars %>%
  bam(
    data = .,
    formula = complete_pass ~
      s(air_yards, k = 12) + s(ydstogo, k = 12) + s(yardline_100, k = 12) + s(wp) + s(ep) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100) + ti(ydstogo, yardline_100) +
      ti(air_yards, ydstogo, by = factor(down)) + ti(air_yards, yardline_100, by = factor(down)) +
      ti(ydstogo, yardline_100, by = factor(down)) + factor(down) +
      factor(receiver_position) + factor(pass_is_middle) +
      factor(tipped) + factor(hail_mary) + factor(roof) + factor(posteam_type) + week + season + 
      factor(qb_hit) + factor(shotgun),
    family = binomial()
  )

gam_pred_full <- predict.bam(gam_full, newdata = model_vars, type = "response")

gam_pred_full_data <- data.frame("complete_pass" = model_vars$complete_pass, "pred" = gam_pred_full)
gam_pred_full_data %<>% mutate(error = complete_pass - pred)
gam_pred_full_data %>% summarise(mean_abs_error = mean(abs(error)),
                            mean_sq_error = mean(error^2),
                            root_mean_sq_error = sqrt(mean(error^2)))



# Just Air Yards ----------------------------------------------------------


gam_air <- train_gam %>%
  bam(data = ., formula = complete_pass ~ s(air_yards), family = binomial())

#  ** Air Yards Summary

summary(gam_air)

# Test Air Yards -----------
gam_pred_air <- predict.bam(gam_air, newdata = test_gam, type = "response")


gam_pred_air_data <- data.frame("complete_pass" = test_gam$complete_pass, "pred" = gam_pred_air)
gam_pred_air_data %<>% mutate(error = complete_pass - pred)
gam_pred_air_data %>% summarise(mean_abs_error = mean(abs(error)),
                            mean_sq_error = mean(error^2),
                            root_mean_sq_error = sqrt(mean(error^2)))

# Trained on All Data ---------
gam_air_full <- model_vars %>%
  bam(data = ., formula = complete_pass ~ s(air_yards), family = binomial())

gam_pred_air_full <- predict.bam(gam_air_full, newdata = model_vars, type = "response")

gam_pred_air_full_data <- data.frame("complete_pass" = model_vars$complete_pass, "pred" = gam_pred_air_full)
gam_pred_air_full_data %<>% mutate(error = complete_pass - pred)
gam_pred_air_full_data %>% summarise(mean_abs_error = mean(abs(error)),
                                mean_sq_error = mean(error^2),
                                root_mean_sq_error = sqrt(mean(error^2)))
