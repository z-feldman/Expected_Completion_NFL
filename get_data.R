library(tidyverse)
library(stringr)
library(nflscrapR)
source("plays-functions.R")




pbp09 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2009.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2009.csv"))
pbp10 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2010.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2010.csv"))
pbp11 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2011.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2011.csv"))
pbp12 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2012.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2012.csv"))
pbp13 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2013.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2013.csv"))
pbp14 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2014.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2014.csv"))
pbp15 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2015.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2015.csv"))
pbp16 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2016.csv"))
pbp17 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2017.csv"))
pbp18 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2018.csv"))
pbp19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")

pbp09 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp10 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp11 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp12 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp13 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp14 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp15 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp16 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp17 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp18 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)
pbp19 %<>% fix_inconsistent_data_types() %>% fix_team_abbreviations(TRUE)

ros09 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2009.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2009.csv"))
ros10 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2010.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2010.csv"))
ros11 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2011.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2011.csv"))
ros12 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2012.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2012.csv"))
ros13 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2013.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2013.csv"))
ros14 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2014.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2014.csv"))
ros15 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2015.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2015.csv"))
ros16 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2016.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2016.csv"))
ros17 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2017.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2017.csv"))
ros18 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2018.csv"),
               read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/post_season/post_roster_2018.csv"))
ros19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2019.csv")

games09 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2009.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2009.csv"))
games10 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2010.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2010.csv"))
games11 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2011.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2011.csv"))
games12 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2012.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2012.csv"))
games13 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2013.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2013.csv"))
games14 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2014.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2014.csv"))
games15 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2015.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2015.csv"))
games16 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2016.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2016.csv"))
games17 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2017.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2017.csv"))
games18 <- rbind(read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2018.csv"),
                 read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_2018.csv"))
games19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_2019.csv")
                 

plays <- bind_rows(pbp09, pbp10, pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18, pbp19)
rosters <- bind_rows(ros09, ros10, ros11, ros12, ros13, ros14, ros15, ros16, ros17, ros18, ros19)
games <- bind_rows(games09, games10, games11, games12, games13, games14, games15, games16, games17, games18, games19)


rm(pbp09, pbp10, pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18, pbp19,
   ros09, ros10, ros11, ros12, ros13, ros14, ros15, ros16, ros17, ros18, ros19,
   games09, games10, games11, games12, games13, games14, games15, games16, games17, games18, games19)


plays %<>% fix_team_abbreviations(old_to_new = T)
plays %<>% apply_baldwin_mutations()
plays %<>% apply_completion_probability(all_plays = plays)
plays %<>% apply_name_fixes()
plays %<>% apply_sharpe_mutations()

plays_season_week_type <- left_join(plays, games %>% select(type, season, week, game_id), by = "game_id")
plays <- plays_season_week_type
rm(plays_season_week_type)

names(rosters)[which(names(rosters) == "abbr_player_name")] <- "name"
rosters %<>% apply_roster_name_fixes()
rosters %<>% fix_team_abbreviations(old_to_new = T)

plays_ros_qb <- left_join(plays, rosters %>% select(season, team, season_type,gsis_id, passer_player_position = position), 
                          by = c("season", "posteam" = "team", "passer_player_id" = "gsis_id", "type" = "season_type"))
plays_ros_qb_wr <- left_join(plays_ros_qb, rosters %>% select(season, team, gsis_id, season_type, receiver_player_position = position), 
                             by = c("season", "posteam" = "team", "receiver_player_id" = "gsis_id", "type" = "season_type"))
plays <- plays_ros_qb_wr
rm(plays_ros_qb, plays_ros_qb_wr)

