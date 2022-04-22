library(here)
library(glue)
library(tidyverse)
library(lubridate)

source(here("data", "utils.R"))

data_dir <- "data"

seasons <- c("2018", "2019", "2020", "2021")
seasons_short <- map_chr(seasons, ~ str_trunc(.x, width = 3, side = "left", ellipsis = "'"))
seasons_short <- paste0(seasons_short, collapse = "")

##-----------------------------------------------------------------------------#
## SOG data --------------------------------------------------------------------
##-----------------------------------------------------------------------------#

## Pull in data ---------------------------------------------------------------#
out <- build_sog_data(seasons)

## Save data to .rds ----------------------------------------------------------#
fname = glue("sog-data_s{seasons_short}_{today()}.rds")
saveRDS(out, here(data_dir, fname))


##-----------------------------------------------------------------------------#
## SOG Model data --------------------------------------------------------------
##-----------------------------------------------------------------------------#

## Shots on goal --------------------------------------------------------------#
o1 <- c("n_shot", "n_goal")
out1 <- build_sog_model_data(seasons, outcome = o1)
check_sog_model_data(out1)

fname = glue("sog-model-data_o-{short_outcome(o1)}_s{seasons_short}_{today()}.rds")
saveRDS(out1, here(data_dir, fname))

## Blocked/Missed shots -------------------------------------------------------#
o2 <- c("n_missshot", "n_blkdshot")
out2 <- build_sog_model_data(seasons, outcome = o2)
check_sog_model_data(out2)

fname = glue("sog-model-data_o-{short_outcome(o2)}_s{seasons_short}_{today()}.rds")
saveRDS(out2, here(data_dir, fname))

