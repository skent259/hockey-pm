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
#' @argument `i` an integer specifying which cv row to run
#' @argument `cv_spec` name of the cross-validation specification to use
#' @argument `condor` logical, whether to run on condor system (bcg only)
#' @argument `output_dir` A character vector specifying the output directory
args <- commandArgs(trailingOnly = TRUE)
print(args)

i <- as.integer(args[1]) + 1
cv_spec <- args[2]
condor <- isTRUE(as.logical(args[3]))
output_dir <- args[4]

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if (is.na(.x)) val else .x 
}
i <- set_default(i, 1)
cv_spec <- set_default(cv_spec, "model/shots/cv/cv-spec_2022-05-19.rds")
condor <- set_default(condor, FALSE)
output_dir <- set_default(output_dir, "model/shots/cv/output")

print(list(i = i, cv_spec = cv_spec, condor = condor, output_dir = output_dir))

## Pull in information from cv_spec -------------------------------------------#

cv_spec <- readRDS(here(cv_spec))
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

## Set up other folders -------------------------------------------------------#

nt_flag <- ifelse(team, "t", "nt")
chkpt_folder_nm <- glue::glue("chkpt_cv-ppool_{i}_{outcome}_{seasons}_{nt_flag}_f{fold}")
chkpt_folder <- here(output_dir, chkpt_folder_nm)
if (!dir.exists(chkpt_folder)) {
  chkpt_folder <- create_folder(chkpt_folder_nm, path = here(output_dir))
}

if (condor) {
  cmdstanr::set_cmdstan_path("/ua/spkent/.cmdstan/cmdstan-2.29.2") 
} else {
  # rstan_options(auto_write = TRUE)
  cmdstanr::set_cmdstan_path()
}

print(chkpt_folder_nm)

## Run model and save ---------------------------------------------------------#

model_file <- ifelse(team, "ppool_cv.stan", "ppool_nt_cv.stan")
model_file <- here("model/shots/cv", model_file)
pm_mod <- readr::read_lines(model_file)
datalist <- make_datalist_ppool_shots(d_train, outcome, team)
output_fname <- glue::glue("cv-ppool_{i}_{outcome}_{seasons}_{nt_flag}_{lubridate::today()}.rds")

if (!file.exists(here(output_dir, output_fname))) {
  pm_fit <- chkpt_stan(model_code = pm_mod, 
                      data = datalist, 
                      iter_per_chkpt = 50,
                      parallel_chains = 4,
                      path = chkpt_folder)

  draws <- combine_chkpt_draws(object = pm_fit)

  saveRDS(draws, here(output_dir, output_fname))
} else {
  draws <- readRDS(here(output_dir, output_fname))
}

## Get predictions on testing data --------------------------------------------#
datalist_test <- make_datalist_ppool_shots(d_test, outcome, team)

pm_mod <- cmdstanr::cmdstan_model(model_file)
gq <- pm_mod$generate_quantities(draws, data = datalist_test)

gq_summary <- gq$summary(
  variables = "test_pred",
  mean, 
  sd, 
  ~my_quantile(.x, probs = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975))
)

# join as list column in cv_spec, save the cv_spec row 
cv_spec_i <-
  cv_spec[i, ] %>%
  dplyr::mutate(gq_summary = list(gq_summary))

output_fname <- glue::glue("cv-ppool_{i}_{outcome}_{seasons}_{nt_flag}_gq_{lubridate::today()}.rds")
saveRDS(cv_spec_i, here(output_dir, output_fname))


# pm_mod <- stan_model(here("model/shots", model_file))
# pm_fit <- sampling(object = pm_mod,
#                    data = datalist)


