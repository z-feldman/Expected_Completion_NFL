# Load Libraries ----------------------------------------------------------

library(mgcv)
library(magrittr)
library(gratia)
library(tidyverse)
library(cowplot)
library(randomForest)
library(broom)
library(xgboost)
library(Ckmeans.1d.dp)
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
# Tidy Data ---------------------------------------------------------------


plays <- readRDS("playdata")
plays %<>% apply_name_fixes()
positions <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/nfl_rosters")
positions %<>% mutate(
  abbr_player_name = ifelse(abbr_player_name == "G.Minshew II", "G.Minshew", abbr_player_name),
  abbr_player_name = ifelse(abbr_player_name == "Jos.Allen", "J.Allen", abbr_player_name),
  abbr_player_name = ifelse(abbr_player_name == "D.Chark Jr.", "D.Chark", abbr_player_name))

positions %<>% 
  filter(season >= 2009) %>% fix_team_abbreviations(old_to_new = T) %>% 
  mutate(position = if_else(position == "HB", "RB", position))

plays %<>% mutate(season_type = if_else(week >= 18, "post", "reg"))

plays %<>% left_join(
                   y = positions %>% select(season, season_type, abbr_player_name, team, receiver_position = position, gsis_id),
                   by = c("season", "season_type", "receiver_player_name" = "abbr_player_name", "posteam" = "team", "receiver_player_id" = "gsis_id"))
plays %<>% left_join(
                   y = positions %>% select(season, season_type, abbr_player_name, team, passer_position = position, gsis_id),
                   by = c("season", "season_type", "passer_player_name" = "abbr_player_name", "posteam" = "team", "passer_player_id" = "gsis_id"))
plays %<>% left_join(
               y = positions %>% select(season, season_type, abbr_player_name, team, rusher_position = position, gsis_id),
               by = c("season", "season_type", "rusher_player_name" = "abbr_player_name", "posteam" = "team", "rusher_player_id" = "gsis_id"))

cpoe_plays <- plays %>%
  filter(play == 1 & (complete_pass == 1 |
           incomplete_pass == 1 | interception == 1)) %>%
  filter(
    qb_spike == 0,
    air_yards >= -10,
    air_yards <= 55,
    ydstogo <= 35,
    !is.na(receiver_player_id),
    !is.na(pass_location)
  ) %>%
  filter(!str_detect(desc, fixed("thrown away", ignore_case = TRUE)),
         !str_detect(desc, fixed("throw away", ignore_case = TRUE)),
         !str_detect(desc, fixed("hail mary", ignore_case = TRUE))) %>%
  filter(passer_position == "QB",
         receiver_position %in% c("FB", "RB", "WR", "TE")) %>%
  mutate(
    passer_is_qb = if_else(passer_position == "QB", 1, 0),
    receiver_is_wr = if_else(receiver_position == "WR", 1, 0),
    receiver_is_te = if_else(receiver_position == "TE", 1, 0),
    receiver_is_rb = if_else(receiver_position == "RB", 1, 0),
    receiver_is_fb = if_else(receiver_position == "FB", 1, 0),
    receiver_is_none = if_else(!(receiver_position %in% c("WR", "TE", "RB", "FB")), 1, 0),
    air_is_zero = if_else(air_yards == 0, 1, 0),
    pass_is_middle = if_else(pass_location == "middle", 1, 0),
    under_two_min = if_else(half_seconds_remaining <= 120, 1, 0),
    early_downs = if_else(down < 3, 1, 0),
    tipped = if_else(str_detect(desc, fixed("knocked", ignore_case = TRUE)) | str_detect(desc, fixed("batted", ignore_case = TRUE)) | str_detect(desc, fixed("knocked", ignore_case = TRUE)), 1, 0)
  )



saveRDS(cpoe_passes, "cpoe_passes")
rm(plays, positions, team_colors, team_logos)
# Partition Data ----------------------------------------------------------

set.seed(69)
plays %<>% mutate(id = row_number())
train <- plays %>% sample_frac(0.70)
test <- anti_join(plays, train, by = 'id')

# Model 1 -----------------------------------------------------------------
model1 <- plays %>%
  bam(
    data = .,
    formula = complete_pass ~ factor(season) + s(air_yards) + s(ydstogo) + s(yardline_100) + s(wp) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100, by = factor(receiver_position)) + ti(ydstogo, yardline_100, by = factor(receiver_position)) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) +
      factor(early_downs) + factor(receiver_position) + factor(pass_is_middle) + factor(air_is_zero) + factor(under_two_min),
    family = binomial()
  )

summary1 <- summary(model1)
summary1

pred1 <- predict.bam(model1, newdata = test, se.fit = TRUE, type = "link")


plot_pred1 <- data.frame("complete_pass" = test$complete_pass, "logit" = pred1$fit, "se" = pred1$se.fit)
plot_pred1 %<>% mutate(prob = plogis(logit),
                       high_prob = plogis(logit + se*2),
                       low_prob = plogis(logit- se*2))


plot_pred1 %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred1 %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred1 %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))
  
saveRDS(model1, "model1")
saveRDS(plot_pred1, "plot_pred1")
write_csv(train, "train.csv")
write_csv(test, "test.csv")

# Model 2 -----------------------------------------------------------------

#  ** Train 2 

model2 <- plays %>%
  bam(
    data = .,
    formula = complete_pass ~ factor(season) + s(air_yards) + s(ydstogo) + s(yardline_100) + s(wp) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100, by = factor(receiver_position)) + ti(ydstogo, yardline_100, by = factor(receiver_position)) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) +
      factor(early_downs) + factor(receiver_position) + factor(pass_is_middle) + factor(air_is_zero) + factor(under_two_min) + factor(tipped),
    family = binomial()
  )


#  ** Summary, Appraise, Check 2 


summary2 <- summary(model2)
summary2

appraise(model2, type = "response")
mgcv::gam.check(model2)


tidy2 <- broom::tidy(model2)
glance(model2)


#  ** Predict 2


pred2 <- predict.bam(model2, newdata = test, se.fit = TRUE, type = "link")


plot_pred2 <- data.frame("complete_pass" = test$complete_pass, "logit" = pred2$fit, "se" = pred2$se.fit)
plot_pred2 %<>% mutate(prob = plogis(logit),
                       high_prob = plogis(logit + se*2),
                       low_prob = plogis(logit- se*2))


plot_pred2 %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred2 %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred2 %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))



# Model 3 -----------------------------------------------------------------
#  ** Train 3 
model3 <- plays %>%
  bam(
    data = .,
    formula = complete_pass ~ factor(season) + s(air_yards, k = 12) + s(ydstogo) + s(yardline_100) + s(wp) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100, by = factor(receiver_position), k = c(16, 16, 16, 20)) + ti(ydstogo, yardline_100, by = factor(receiver_position)) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) +
      factor(early_downs) + factor(receiver_position) + factor(pass_is_middle) + factor(air_is_zero) + factor(under_two_min) + factor(tipped),
    family = binomial()
  )


#  ** Summary, Appraise, Check 3


summary3 <- summary(model3)
summary3

appraise(model3, type = "response")
mgcv::k.check(model3)

plot(model3)
tidy2 <- broom::tidy(model3)
glance(model3)


#  ** Predict 3


pred3 <- predict.bam(model3, newdata = test, se.fit = TRUE, type = "link")


plot_pred3 <- data.frame("complete_pass" = test$complete_pass, "logit" = pred3$fit, "se" = pred3$se.fit)
plot_pred3 %<>% mutate(prob = plogis(logit),
                       high_prob = plogis(logit + se*3),
                       low_prob = plogis(logit- se*3))


plot_pred3 %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred3 %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred3 %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))



# Model 4 -----------------------------------------------------------------
#  ** Train 4
model4 <- cpoe_passes %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards, k = 12) + s(ydstogo, k = 12) + s(yardline_100, k = 12) + s(wp) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100) + ti(ydstogo, yardline_100) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) + factor(early_downs) + 
      factor(receiver_position) + factor(pass_is_middle) + 
      factor(under_two_min) + factor(tipped),
    family = binomial()
  )


model4_log <- cpoe_passes %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards, k = 12) + s(log(ydstogo), k = 12) + s(yardline_100, k = 12) + s(wp) + ti(air_yards, log(ydstogo), by = factor(receiver_position)) +
      ti(air_yards, yardline_100) + ti(log(ydstogo), yardline_100) +
      ti(air_yards, log(ydstogo), by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) + factor(early_downs) + 
      factor(receiver_position) + factor(pass_is_middle) + 
      factor(under_two_min) + factor(tipped),
    family = binomial()
  )


model4_gaussian <- cpoe_passes %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards, k = 12) + s(ydstogo, k = 12) + s(yardline_100, k = 12) + s(wp) + ti(air_yards, ydstogo, by = factor(receiver_position)) +
      ti(air_yards, yardline_100) + ti(ydstogo, yardline_100) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) +
      factor(early_downs) + factor(receiver_position) + factor(pass_is_middle) + factor(air_is_zero) + 
      factor(under_two_min) + factor(tipped)
  )
saveRDS(model4, "model4")
saveRDS(model4_log, "model4_log")
saveRDS(model4_gaussian, "model4_gaussian")

#  ** Summary, Appraise, Check 4

model4 <- readRDS("model4")
summary4 <- summary(model4)
summary4

appraise(model4, type = "response")
mgcv::k.check(model4)

plot(model4, pages = 2)
tidy2 <- broom::tidy(model4)
glance(model4)

#  ** Summary, Appraise, Check 4_gauss

model4_gaussian <- readRDS("model4_gaussian")
summary4_gauss <- summary(model4_gaussian)
summary4_gauss

appraise(model4_gaussian)
mgcv::k.check(model4_gaussian)



plot(model4, pages = 2)
tidy2 <- broom::tidy(model4)
glance(model4)

#  ** Predict 4


pred4 <- predict.bam(model4, newdata = cpoe_passes, se.fit = T, type = "link")
pred4_log <- predict.bam(model4_log, newdata = cpoe_passes, se.fit = T, type = "link")
pred4_gauss <- predict.bam(model4_gaussian, newdata = cpoe_passes, se.fit = T)

saveRDS(pred4, "pred4")
saveRDS(pred4_log, "pred4_log")
saveRDS(pred4_gauss, "pred4_gauss")
plot_pred4 <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred4$fit, "se" = pred4$se.fit)
plot_pred4 %<>% mutate(prob = prediction,
                       high_prob = prediction + se*2,
                       low_prob = prediction + se*2)


plot_pred4 %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred4 %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred4 %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))




plot_pred4_gaus <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred4_gaus$fit, "se" = pred4_gaus$se.fit)
plot_pred4_gaus %<>% mutate(prob = prediction,
                       high_prob = prediction + se*2,
                       low_prob = prediction + se*2)


plot_pred4_gaus %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred4_gaus %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred4_gaus %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))



# Model 5 --------------------------------
model5 <- plays %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards, k = 12) + s(ydstogo, k = 12) + s(yardline_100, k = 12) +
      ti(air_yards, yardline_100) + ti(ydstogo, yardline_100) +
      ti(air_yards, ydstogo, by = factor(early_downs)) + ti(air_yards, yardline_100, by = factor(early_downs)) +
      ti(ydstogo, yardline_100, by = factor(early_downs)) +
      factor(season) + factor(early_downs) + factor(pass_is_middle) + factor(air_is_zero) +
      factor(under_two_min)
  )
saveRDS(model5, "model5")

#  ** Summary, Appraise, Check 5

model5 <- readRDS("model5")
summary5 <- summary(model5)
summary5

appraise(model5, type = "response")
mgcv::k.check(model5)

plot(model5, pages = 2)
tidy2 <- broom::tidy(model5)
glance(model5)

#  ** Predict 5


pred5 <- predict.bam(model5, newdata = plays, se.fit = T)

plot_pred5 <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred5$fit, "se" = pred5$se.fit)
plot_pred5 %<>% mutate(prob = prediction,
                       high_prob = prediction + se*2,
                       low_prob = prediction + se*2)


plot_pred5 %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_pred5 %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_pred5 %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                         rmse = mean(sq_error) %>% sqrt() %>% round(3),
                         best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))




# Model 6 -----------------------------
model6 <- cpoe_passes %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards) + s(log(ydstogo)) + s(yardline_100) + ti(air_yards, log(ydstogo)) +
      ti(air_yards, yardline_100) + ti(log(ydstogo), yardline_100) + factor(early_downs) + 
      factor(receiver_position) + factor(pass_is_middle) + 
      factor(under_two_min) + factor(tipped),
    family = binomial()
  )


model6_gaussian <- cpoe_passes %>%
  bam(
    data = .,
    formula = complete_pass ~ 
      s(air_yards) + s(log(ydstogo)) + s(yardline_100) + ti(air_yards, log(ydstogo)) +
      ti(air_yards, yardline_100) + ti(log(ydstogo), yardline_100) + factor(early_downs) + 
      factor(receiver_position) + factor(pass_is_middle) + 
      factor(under_two_min) + factor(tipped)
  )
saveRDS(model6, "model6")
saveRDS(model6_gaussian, "model6_gaussian")

pred6 <- predict.bam(model6, newdata = cpoe_passes, se.fit = T, type = "link")
pred6_gauss <- predict.bam(model6_gaussian, newdata = cpoe_passes, se.fit = T)

saveRDS(pred6, "pred6")
saveRDS(pred6_gauss, "pred6_gaussian")

# Ben's Model -------------------------------------------------------------

ben_ec <- cpoe_passes %>%
  bam(data = ., formula = complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero + 
        factor(down) + factor(pass_location), family = binomial())

ben_ec_gaussian <- cpoe_passes %>%
  bam(data = ., formula = complete_pass ~ s(air_yards) + s(yardline_100) +log(ydstogo) + air_is_zero + 
        factor(down) + factor(pass_location))



saveRDS(ben_ec, "ben_ec")
saveRDS(ben_ec_gaussian, "ben_ec_gaussian")

summary_ben <- summary(ben_ec)
summary_ben
gratia::draw(ben_ec)
pred_ben <- predict.bam(ben_ec, newdata = cpoe_passes, se.fit = TRUE, type = "link")
pred_ben_gauss <- predict.bam(ben_ec_gaussian, newdata = cpoe_passes, se.fit = TRUE)

saveRDS(pred_ben, "pred_ben")
saveRDS(pred_ben_gauss, "pred_ben_gauss")
plot_ben <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred_ben$fit, "se" = pred_ben$se.fit)
plot_ben %<>% mutate(prob = prediction,
                       high_prob = prediction + se*2,
                       low_prob = prediction - se*2)
plot_ben %<>% mutate(sq_error = (complete_pass - prob)^2,
                       high_sq_error = (complete_pass - high_prob)^2,
                       low_sq_error = (complete_pass - low_prob)^2)
plot_ben %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                       best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_ben %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                       rmse = mean(sq_error) %>% sqrt() %>% round(3),
                       best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))

# Josh Model --------------------------------
josh_ec <- plays %>%
  bam(data = ., formula = complete_pass ~ s(air_yards) + factor(down) + factor(pass_location) + s(ydstogo) + factor(season) + factor(air_is_zero), method = "REML")

summary_josh <- summary(josh_ec)
summary_josh

gratia::draw(josh_ec)

mgcv::k.check(josh_ec)

pred_josh <- predict.bam(josh_ec, newdata = plays, se.fit = TRUE)

plot_josh <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred_josh$fit, "se" = pred_josh$se.fit)
plot_josh %<>% mutate(prob = prediction,
                     high_prob = prediction + se*2,
                     low_prob = prediction - se*2)
plot_josh %<>% mutate(sq_error = (complete_pass - prob)^2,
                     high_sq_error = (complete_pass - high_prob)^2,
                     low_sq_error = (complete_pass - low_prob)^2)
plot_josh %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                     best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_josh %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                       rmse = mean(sq_error) %>% sqrt() %>% round(3),
                       best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))
# Just Air Yards ----------------------------------------------------------


air_ec <- cpoe_passes %>%
  bam(data = ., formula = complete_pass ~ s(air_yards), family = binomial())

air_ec_air_zero <- cpoe_passes %>%
  bam(data = .,formula = complete_pass ~ s(air_yards) + factor(air_is_zero), family = binomial())

air_ec_gaussian <- cpoe_passes %>%
  bam(data =., formula = complete_pass ~ s(air_yards))

saveRDS(air_ec, "air_ec")
saveRDS(air_ec_air_zero, "air_ec_air_zero")
saveRDS(air_ec_gaussian, "air_ec_gassian")

#  * Air Yards Summary -


air_ec_summary <- summary(air_ec)
air_ec_summary

summary_air <- summary(model_air)
summary_air


#  * Air Yards Predictions 

pred_air <- predict.bam(air_ec, newdata = cpoe_passes, se.fit = TRUE, type = "link")
pred_air_zero <- predict.bam(air_ec_air_zero, newdata = cpoe_passes, se.fit = TRUE, type = "link")
pred_air_gauss <- predict.bam(air_ec_gaussian, newdata = cpoe_passes, se.fit = TRUE)

saveRDS(pred_air, "pred_air")
saveRDS(pred_air_zero, "pred_air_zero")
saveRDS(pred_air_gauss, "pred_air_gauss")

#  * Air Yards Predictions df and Investigating 


plot_air <- data.frame("complete_pass" = plays$complete_pass, "prediction" = pred_air$fit, "se" = pred_air$se.fit)
plot_air %<>% mutate(prob = prediction,
                      high_prob = prediction + se*2,
                      low_prob = prediction - se*2)
plot_air %<>% mutate(sq_error = (complete_pass - prob)^2,
                      high_sq_error = (complete_pass - high_prob)^2,
                      low_sq_error = (complete_pass - low_prob)^2)
plot_air %<>% mutate(worst_sq_error = if_else(high_sq_error > low_sq_error, high_sq_error, low_sq_error),
                      best_sq_error = if_else(high_sq_error < low_sq_error, high_sq_error, low_sq_error))

plot_air %>% summarise(worst_rmse = mean(worst_sq_error) %>% sqrt() %>% round(3),
                        rmse = mean(sq_error) %>% sqrt() %>% round(3),
                        best_rmse = mean(best_sq_error) %>% sqrt() %>% round(3))



# Just Mean Completion of Train -------------------------------------------

mean <- mean(train$complete_pass, na.rm = TRUE)

plot_mean <- data.frame("complete_pass" = test$complete_pass, prob = mean)
plot_mean %<>% mutate(sq_error = (complete_pass - prob)^2)

plot_mean %>% summarise(rmse = mean(sq_error) %>% sqrt() %>% round(3))





# Random Forest for Classification ----------------------------------------


plays %<>%
  filter(complete_pass == 1 |
           incomplete_pass == 1 | interception == 1) %>%
  filter(
    qb_spike == 0,
    air_yards >= -10,
    air_yards <= 55,
    ydstogo <= 35,
    !is.na(receiver_player_id),
    !is.na(pass_location)
  ) %>%
  filter(!str_detect(desc, fixed("thrown away", ignore_case = TRUE)),
         !str_detect(desc, fixed("throw away", ignore_case = TRUE)),
         !str_detect(desc, fixed("hail mary", ignore_case = TRUE))) %>%
  filter(passer_position == "QB",
         receiver_position %in% c("FB", "RB", "WR", "TE")) %>%
  mutate(
    air_is_zero = factor(if_else(air_yards == 0, 1, 0)),
    pass_is_middle = factor(if_else(pass_location == "middle", 1, 0)),
    under_two_min = factor(if_else(half_seconds_remaining <= 120, 1, 0)),
    early_downs = factor(if_else(down < 3, 1, 0)),
    complete_pass = factor(complete_pass),
    receiver_position = factor(receiver_position),
    passer_position = factor(passer_position),
    season = factor(season)
  )

plays %<>% 
  select(complete_pass, air_yards, ydstogo, 
         yardline_100, wp, receiver_position, 
         air_is_zero,
         pass_is_middle, 
         under_two_min, 
         early_downs,
         season)
plays_train <- sample(nrow(plays), 0.7*nrow(plays), replace = FALSE)
train <- plays[plays_train,]
test <- plays[-plays_train,]
model1 <- randomForest(factor(complete_pass) ~ ., data = train, 
                       importance = TRUE, na.action = na.omit, mtry = 4, ntree = 1001)

pred <- predict(model1, train, type = "class")
mean(pred == train$complete_pass)
table(pred, train$complete_pass)

expected_completion_random_forest <- model1
expected_completion_random_forest
varImpPlot(expected_completion_random_forest)
importance(expected_completion_random_forest)

pred_test <- predict(model1, test, type = "class")
table(pred_test, test$complete_pass)

mean(pred_test == test$complete_pass)



# Inspecting Plays df -----------------------------------------------------

plays %>% 
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1) %>%
  filter(qb_spike == 0) %>%
  filter(!is.na(receiver_player_id)) %>%
  filter(!str_detect(desc, "knocked") & !str_detect(desc,"batted") & !str_detect(desc, "thrown away") & !str_detect(desc, "Thrown away") & !str_detect(desc, "tipped") & !str_detect(desc, "throw away")) %>%
  filter(air_yards >= -10 & air_yards <= 55) %>%
  filter(home_team != "CIN") %>% filter(air_yards == 0, complete_pass == 0) %>% View()
  group_by(air_yards) %>%
  mutate(completion_perc = mean(complete_pass, na.rm = TRUE)) %>%
  ggplot(aes(x = air_yards, y = completion_perc)) +
  geom_point()


plays %>% filter(air_yards >= 55) %>% select(qtr, half_seconds_remaining, score_differential, desc, air_yards) %>% View()
 
