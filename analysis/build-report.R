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

# model_fname <- "ppool_mi-bl_s'18_2022-05-20.rds"
# data_fname <- "sog-model-data_o-sh-go_s'18_2022-05-03.rds"
out_dir <- "analysis/reports"

params <- tibble::tribble(
  ~model_fname, ~data_fname, 
  "ppool_mi-bl_nt_s'18_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'18_2022-05-03.rds",
  "ppool_mi-bl_nt_s'19_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'19_2022-05-03.rds",
  "ppool_mi-bl_nt_s'20_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'20_2022-05-03.rds",
  "ppool_mi-bl_nt_s'21_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'21_2022-04-25.rds",
  "ppool_mi-bl_s'18_2022-05-20.rds",	"sog-model-data_o-sh-go_s'18_2022-05-03.rds",
  "ppool_mi-bl_s'19_2022-05-18.rds",	"sog-model-data_o-sh-go_s'19_2022-05-03.rds",
  "ppool_mi-bl_s'20_2022-05-19.rds",	"sog-model-data_o-sh-go_s'20_2022-05-03.rds",
  "ppool_mi-bl_s'21_2022-05-19.rds",	"sog-model-data_o-sh-go_s'21_2022-04-25.rds",
  "ppool_sh-go_nt_s'18_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'18_2022-05-03.rds",
  "ppool_sh-go_nt_s'19_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'19_2022-05-03.rds",
  "ppool_sh-go_nt_s'20_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'20_2022-05-03.rds",
  "ppool_sh-go_nt_s'21_2022-05-19.rds",	"sog-model-data_o-mi-bl_s'21_2022-04-25.rds",
  "ppool_sh-go_s'18_2022-05-19.rds",	"sog-model-data_o-sh-go_s'18_2022-05-03.rds",
  "ppool_sh-go_s'19_2022-05-19.rds",	"sog-model-data_o-sh-go_s'19_2022-05-03.rds",
  "ppool_sh-go_s'20_2022-05-19.rds",	"sog-model-data_o-sh-go_s'20_2022-05-03.rds",
  "ppool_sh-go_s'21_2022-05-18.rds",	"sog-model-data_o-sh-go_s'21_2022-04-25.rds"
)

## Build report ---------------------------------------------------------------#
build_report <- function(model_fname, data_fname, out_dir) {
  mod_dir <- "model/shots/output"
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
}

for (i in seq_len(nrow(params))) {
  model_fname <- params$model_fname[[i]]
  data_fname <- params$data_fname[[i]] 
  build_report(model_fname, data_fname, out_dir)
}
