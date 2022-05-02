##-----------------------------------------------------------------------------#
#' Build report file 
#' 
#' Run this R script to build the report "model-report-template.Rmd" based on
#specified parameters.  This is preferable because it will make sure to append
#the date that it was run
##-----------------------------------------------------------------------------#

library(here)
source(here("analysis/utils.R"))

## Change these parameters ----------------------------------------------------#

model_fname <- "ppool_sh-go_s'21_2022-04-30.rds"
data_fname <- "sog-model-data_o-sh-go_s'21_2022-04-25.rds"
out_dir <- "analysis/reports"

## Build report ---------------------------------------------------------------#
mod_dir <- "model/shots"
data_dir <- "data"

temp_fname <- here("analysis/reports", "model-report-template-shots.Rmd")

params <- list(
  model_fname = here(mod_dir, model_fname),
  data_fname = here(data_dir, data_fname)
)

make_report_fname <- function(model_fname, data_fname) {
  model <- pull_modname(model_fname)
  seasons <- pull_seasons(data_fname)
  glue::glue("model-report_{model}_{seasons}_{lubridate::today()}.html")  
}
rep_fname <- make_report_fname(model_fname, data_fname)


out <- rmarkdown::render(
  temp_fname,
  params = params,
  output_format = "html_document",
  output_file = here(out_dir, rep_fname)
)
