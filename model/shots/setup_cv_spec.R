library(tidyverse)
library(here)
library(glue)
library(rsample)

source(here("analysis/utils.R"))
data_dir <- "data"

## CV specification parameters ---------------------------------------------------#

n_folds <- 10
n_rep <- 1

d_fnames <- c(
  "sog-model-data_o-mi-bl_s'21_2022-04-25.rds",
  "sog-model-data_o-sh-go_s'21_2022-04-25.rds"
)
d_fnames <- glue("{data_dir}/{d_fnames}")
team <- c(TRUE, FALSE)


## Get ids of rows in each fold --------------------------------------------------#

#' Folds from rsample::vfold_cv
#' Stratified by "shift_time" in 5 breaks
#' 
#' @param d_fname Name of data set  
#' @return A tibble with 3 columns: `d_fname`, `fold`, `out_id`.  `out_id` is
#'   the rows of data NOT used for training (i.e. used for testing).
get_fold_id_df <- function(d_fname) {
  d <- readRDS(here(d_fname))
  
  # grab pairs of rows together
  even <- 2 * seq_len(nrow(d)/2)
  folds <- as.data.frame(as.matrix(d[even, 1:2])) %>% 
    rsample::vfold_cv(v = n_folds, strata = "shift_time", breaks = 5)
  
  out_ids <- folds$splits %>% 
    map("in_id") %>% 
    map(~setdiff(seq_len(nrow(d)/2), .x)) %>% 
    map(~c(2*.x, 2*.x - 1)) # recover pairs
  
  tibble(
    d_fname = d_fname,
    fold = seq_len(n_folds),
    out_id = out_ids,
    nrow = nrow(d)
  )
}

set.seed(8)
fold_ids <- lapply(d_fnames, FUN = get_fold_id_df)
fold_ids <- do.call(rbind, fold_ids)

## Set up CV specification data set ----------------------------------------------#

cv_spec <- 
  expand_grid(
    rep = seq_len(n_rep),
    fold = seq_len(n_folds),
    d_fname = d_fnames,
    team = team
  ) %>% 
  mutate(
    outcome = map_chr(d_fname, ~pull_outcome(.x)),
    season = map_chr(d_fname, ~pull_seasons(.x)),
  ) %>% 
  left_join(
    fold_ids,
    by = c("d_fname", "fold")
  )

# NOTE: can get in_id from setdiff(seq_len(nrow), out_id). Compute when needed to save memory

out_dir <- "model/shots/cv"
out_fname <- here(out_dir, glue("cv-spec_{lubridate::today()}.rds"))
saveRDS(cv_spec, out_fname)
