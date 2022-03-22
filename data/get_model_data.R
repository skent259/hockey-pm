library(here)
library(glue)
library(tidyverse)
library(lubridate)

source(here("data", "utils.R"))

data_dir <- "data"

## Pull in data ---------------------------------------------------------------#
seasons <- c("2018", "2019", "2020", "2021")
method <- "off-def"

out <- build_model_data(seasons, method)

check_model_data(out, method)


## Save data to .rds ----------------------------------------------------------#

seasons_short <- map_chr(seasons, ~ str_trunc(.x, width = 3, side = "left", ellipsis = "'"))
seasons_short <- paste0(seasons_short, collapse = "")

fname = glue("model-data_s{seasons_short}_{method}_{today()}.rds")
saveRDS(out, here(data_dir, fname))

