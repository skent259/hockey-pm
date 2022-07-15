library(here)
library(glue)
library(readr)
library(purrr)
library(rstan)
# devtools::install_github("https://github.com/skent259/chkptstanr")
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::install_cmdstan()
library(chkptstanr)
library(Matrix)
source(here("analysis/utils.R"))
source(here("model/utils.R"))

options(mc.cores = 4)

## Command line arguments -----------------------------------------------------#
#' @argument `cv_spec` name of the cross-validation specification to use
#' @argument `condor` logical, whether to run on condor system (bcg only)
#' @argument `output_dir` A character vector specifying the output directory
args <- commandArgs(trailingOnly = TRUE)
print(args)

cv_spec <- args[1] %>% 
  set_default("model/shots/cv/cv-spec_2022-05-19.rds")
condor <- isTRUE(as.logical(args[2])) %>% 
  set_default(FALSE)
output_dir <- args[3] %>% 
  set_default("model/shots/cv/output")

print(list(cv_spec = cv_spec, condor = condor, output_dir = output_dir))


## Pull in information  -------------------------------------------------------#

cv_spec <- readRDS(here(cv_spec))

if (condor) {
  cmdstanr::set_cmdstan_path("/ua/spkent/.cmdstan/cmdstan-2.29.2") 
} else {
  # rstan_options(auto_write = TRUE)
  cmdstanr::set_cmdstan_path()
}


## Get generated quantities  --------------------------------------------------#

get_gq_summary <- function(i) {
    row <- transpose(cv_spec[i, ])[[1]]

    rep <- row$rep # not currently used
    fold <- row$fold
    d <- readRDS(here(row$d_fname))
    in_id <- setdiff(seq_len(row$nrow), row$out_id)
    d_train <- d[in_id, ]
    d_test <- d[row$out_id, ]
    outcome <- row$outcome
    seasons <- row$season
    team <- row$team
    nt_flag <- ifelse(team, "t", "nt")

    ## Pull in draws  
    draws_pattern <- glue::glue("cv-ppool_{i}_{outcome}_{seasons}_{nt_flag}_20")
    draws <- readRDS(list.files(output_dir, draws_pattern, full.names = TRUE))

    ## Get predictions on testing data 
    datalist_test <- make_datalist_ppool_shots(d_test, outcome, team)

    model_file <- ifelse(team, "ppool-gq-raw_cv.stan", "ppool-gq-raw_nt_cv.stan")
    model_file <- here("model/shots/cv", model_file)
    gq_mod <- cmdstanr::cmdstan_model(model_file)
    
    gq <- gq_mod$generate_quantities(draws, data = datalist_test)

    gq$summary(
        variables = "log_lambda",
        mean, 
        sd, 
        ~my_quantile(.x, probs = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975))
    )
}

cv_spec$gs_summary <- map(seq_len(nrow(cv_spec)), ~get_gq_summary(.x))


output_fname <- glue::glue("cv-ppool-raw-gq-results.rds")
saveRDS(cv_spec, here(output_dir, output_fname))