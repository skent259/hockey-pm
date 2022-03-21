##-----------------------------------------------------------------------------#
#' Build report file 
#' 
#' Run this R script to build the report "model-report-template.Rmd" based on '
#specified parameters.  This is preferable because it will make sure to append
#the ' date that it was run
##-----------------------------------------------------------------------------#

library(here)
source(here("analysis/utils.R"))

## Change these parameters ----------------------------------------------------#
model_fname <- "ppool_combined.RData"
data_fname <- "model-data_s'18'19'20'21_combined_2022-03-10.rds"
out_dir <- "analysis/reports"

## Build report ---------------------------------------------------------------#
mod_dir <- "model"
data_dir <- "data"

temp_fname <- here("analysis/reports", "model-report-template.Rmd")

params <- list(
  model_fname = here(mod_dir, model_fname),
  data_fname = here(data_dir, data_fname)
)

make_report_fname <- function(model_fname, data_fname) {
  model <- tools::file_path_sans_ext(basename(model_fname))
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
