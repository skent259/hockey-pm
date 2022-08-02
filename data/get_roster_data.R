##-----------------------------------------------------------------------------#
#' Get a roster that matches player names 
#' 
#' Player names from `hockeyR::get_rosters()` don't match those used in the 
#' play-by-play data and so it helps to build a roster data set that contains 
#' the best matches
##-----------------------------------------------------------------------------#

library(tidyverse)
library(hockeyR)
library(stringdist)
library(lubridate)
library(here)

data_dir <- "data"

seasons <- c("2018", "2019", "2020", "2021")
seasons_short <- map_chr(seasons, ~ str_trunc(.x, width = 3, side = "left", ellipsis = "'"))
seasons_short <- paste0(seasons_short, collapse = "")

## Pull in roster -------------------------------------------------------------#

roster <- map_dfr(seasons, ~get_rosters(team = "all", season = .x))

## Add name to match names to model data --------------------------------------#

# initial name modification
roster$player_match <- roster$player %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all(" ", ".")

# pull in sog data
sog_fname <- "sog-model-data_o-sh-go_s'18'19'20'21_2022-04-21.rds"
sog_data <- readRDS(here(data_dir, sog_fname))
sog_players <- unique(colnames(sog_data)[65:ncol(sog_data)])

# use approximate matching since some of the names don't work 
match_ind <- amatch(roster$player_match, sog_players, maxDist = 5)
roster$player_match <- sog_players[match_ind]

## Save results ---------------------------------------------------------------#

fname <- glue("roster-data_s{seasons_short}_{today()}.rds")
saveRDS(roster, here(data_dir, fname))

