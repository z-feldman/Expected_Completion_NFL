library(rvest)
library(glue)
library(tidyverse)
rosters <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv")

missing_roster <- rosters %>% filter(is.na(position))
failed_roster <- missing_roster[-c(1:nrow(missing_roster)),]
failed_roster[1:nrow(missing_roster),] <- NA
get_pos <- function(i){
  missing_roster$position[i] <- read_html(glue("https://www.pro-football-reference.com/players/{(missing_roster$playerid[i]) %>% str_sub(1, 1) %>% str_to_upper()}/{missing_roster$playerid[i]}.htm")) %>% 
    html_node(css = "#meta p:nth-child(3)") %>% 
    html_text() %>% stringr::str_squish() %>% 
    str_sub(str_count(.) -1, str_count(.))
  print(paste0("Finished: ", missing_roster$name[i]))
  print(paste0(nrow(missing_roster) - i, " left!"))
}

store_error <- function(i){
  failed_roster[i,] <- missing_roster[i,]
  print(paste0("Failed: ", missing_roster$name[i]))
  print(paste0(nrow(missing_roster) - i, " left!"))
}

for(player_i in 1:5){
  tryCatch(expr = get_pos(player_i), error = store_error(player_i), finally = next)
  Sys.sleep(5)
}


# roster <- webpage %>% 
#   html_nodes(xpath = '//comment()') %>%    # select comments
#   html_text() %>%    # extract comment text
#   paste(collapse = '') %>%    # collapse to single string
#   read_html() %>%    # reread as HTML
#   html_node('#games_played_team') %>%    # select desired node
#   html_table() 


# LEARN PURRR


