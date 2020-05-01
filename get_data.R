# Load Libraries ----------------------------------------------------------

library(magrittr)
library(tidyverse)
source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

# Get Data ----------------------------------------------------------
pbp09 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2009.csv")
pbp10 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2010.csv")
pbp11 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2011.csv")
pbp12 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2012.csv")
pbp13 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2013.csv")
pbp14 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2014.csv")
pbp15 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2015.csv")
pbp16 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2016.csv")
pbp17 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2017.csv")
pbp18 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2018.csv")
pbp19 <- read_csv("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/pbp2019.csv")

pbp09 %<>% fix_inconsistent_data_types()
pbp10 %<>% fix_inconsistent_data_types()
pbp11 %<>% fix_inconsistent_data_types()
pbp12 %<>% fix_inconsistent_data_types()
pbp13 %<>% fix_inconsistent_data_types()
pbp14 %<>% fix_inconsistent_data_types()
pbp15 %<>% fix_inconsistent_data_types()
pbp16 %<>% fix_inconsistent_data_types()
pbp17 %<>% fix_inconsistent_data_types()
pbp18 %<>% fix_inconsistent_data_types()
pbp19 %<>% fix_inconsistent_data_types()

plays <- bind_rows(pbp09, pbp10, pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18)

rm(pbp09, pbp10, pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18)

# Filter for valid passes -------------------------------------------------

cpoe_passes <- plays %>%
  filter(complete_pass == 1 |
           incomplete_pass == 1 | interception == 1) %>%
  filter(
    qb_spike == 0,
    air_yards >= -10
  ) %>%
  filter(!str_detect(desc, fixed("thrown away", ignore_case = TRUE)),
         !str_detect(desc, fixed("throw away", ignore_case = TRUE))) %>%
  filter(passer_position == "QB",
         receiver_position %in% c("FB", "RB", "WR", "TE"),
         !is.na(pass_location),
         !is.na(receiver_player_id)) %>%
  mutate(
    air_is_zero = if_else(air_yards == 0, 1, 0),
    pass_is_middle = if_else(pass_location == "middle", 1, 0),
    under_two_min = if_else(half_seconds_remaining <= 120, 1, 0),
    early_downs = if_else(down < 3, 1, 0),
    tipped = if_else(str_detect(desc, fixed(" tipped", ignore_case = TRUE)) | str_detect(desc, fixed(" batted", ignore_case = TRUE)) | str_detect(desc, fixed(" bats", ignore_case = TRUE)) | str_detect(desc, fixed(" knocked", ignore_case = TRUE)) | str_detect(desc, fixed(" knocks", ignore_case = TRUE)), 1, 0),
    hail_mary = if_else(str_detect(desc, fixed("hail mary", ignore_case = TRUE)), 1, 0)
  )

saveRDS(cpoe_passes, "data/cpoe_passes")

cpoe_19_passes <- pbp19 %>%
  filter(complete_pass == 1 |
           incomplete_pass == 1 | interception == 1) %>%
  filter(
    qb_spike == 0,
    air_yards >= -10
  ) %>%
  filter(!str_detect(desc, fixed("thrown away", ignore_case = TRUE)),
         !str_detect(desc, fixed("throw away", ignore_case = TRUE))) %>%
  filter(passer_position == "QB",
         receiver_position %in% c("FB", "RB", "WR", "TE"),
         !is.na(pass_location),
         !is.na(receiver_player_id)) %>%
  mutate(
    air_is_zero = if_else(air_yards == 0, 1, 0),
    pass_is_middle = if_else(pass_location == "middle", 1, 0),
    under_two_min = if_else(half_seconds_remaining <= 120, 1, 0),
    early_downs = if_else(down < 3, 1, 0),
    tipped = if_else(str_detect(desc, fixed(" tipped", ignore_case = TRUE)) | str_detect(desc, fixed(" batted", ignore_case = TRUE)) | str_detect(desc, fixed(" bats", ignore_case = TRUE)) | str_detect(desc, fixed(" knocked", ignore_case = TRUE)) | str_detect(desc, fixed(" knocks", ignore_case = TRUE)), 1, 0),
    hail_mary = if_else(str_detect(desc, fixed("hail mary", ignore_case = TRUE)), 1, 0)
  )

saveRDS(cpoe_19_passes, "data/cpoe_19_passes")


