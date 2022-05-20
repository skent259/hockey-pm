library(here)
library(glue)
library(readr)
library(rstan)
# devtools::install_github("https://github.com/skent259/chkptstanr")
# Long story short, I updated this package to work on mac/linux.  See the
# original repo at https://github.com/donaldRwilliams/chkptstanr
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(chkptstanr)
library(Matrix)
library(loo)
source(here("analysis/utils.R"))

# rstan_options(auto_write = TRUE)
cmdstanr::set_cmdstan_path("/ua/spkent/.cmdstan/cmdstan-2.29.2") 
# TODO: make this an argument 
options(mc.cores = 4)

## Command line arguments -----------------------------------------------------#
#' @argument `outcome` Outcome variable to use, options are 'mi-bl' and 'sh-go'
#' @argument `d_fname` name of the data set to use 
args <- commandArgs(trailingOnly = TRUE)
print(args)

outcome <- args[1]
d_fname <- args[2]

#' Set defaults for interactive session 
set_default <- function(.x, val) {
  if (is.na(.x)) val else .x 
}
outcome <- set_default(outcome, "sh-go")
d_fname <- set_default(d_fname, "sog-model-data_o-sh-go_s'21_2022-04-25.rds")
print(d_fname)

## Set up other folders -------------------------------------------------------#

output_dir <- "model/shots/output"
seasons <- pull_seasons(d_fname)

chkpt_folder_nm <- glue::glue("chkpt_{outcome}_{seasons}")
chkpt_folder <- here(output_dir, chkpt_folder_nm)
if (!dir.exists(chkpt_folder)) {
  chkpt_folder <- create_folder(chkpt_folder_nm, path = here(output_dir))
}


## Set up data list for Stan --------------------------------------------------#
d <- readRDS(here("data", d_fname))

ns = nrow(d)/2 #Number of shifts
y <- d[,1] #Number of shots on goal by a given team in a given shift
time = d[,2] #Shift lengths
nt = 31 #Number of teams
names = d@Dimnames[[2]]
np = which(names == names[65])[2]-65 #Number of non-goalie players
ng = ncol(d) - 2*np - 64 #Number of players


#Get Stan's sparse representations of offensive team design matrix 
dTO = d[,seq(3, 63, by=2)]
spVecsTO = rstan::extract_sparse_parts(dTO)
wto = spVecsTO$w
vto = spVecsTO$v
uto = spVecsTO$u
nzt = length(wto)

#Get Stan's sparse representations of defensive team design matrix 
dTD = d[,seq(4, 64, by=2)]
spVecsTD = rstan::extract_sparse_parts(dTD)
wtd = spVecsTD$w
vtd = spVecsTD$v
utd = spVecsTD$u

#Get Stan's representation of offensive player design matrix
dPO = d[,64+1:np]
spVecsPO = rstan::extract_sparse_parts(dPO)
wpo = spVecsPO$w
vpo = spVecsPO$v
upo = spVecsPO$u
nzpo = length(wpo)

#Get Stan's representation of defensive player design matrix (including goalies)
dPD = d[,64+np+1:(np+ng)]
spVecsPD = rstan::extract_sparse_parts(dPD)
wpd = spVecsPD$w
vpd = spVecsPD$v
upd = spVecsPD$u
nzpd = length(wpd)

meanint = switch( # Based on simulation
  outcome,
  "sh-go" = -5, 
  "mi-bl" = -5.5
)
sigmaint = 1
sigmat <- .5
s <-  7.5
r <- 0.5

datalist <- list(ns=ns, y=y, time=time, nt=nt, np=np, ng=ng, wto=wto, vto=vto, 
                 uto=uto, nzt=nzt, wtd = wtd, vtd=vtd, utd=utd, wpo=wpo, vpo=vpo,
                 upo=upo, nzpo=nzpo, wpd=wpd, vpd=vpd, upd=upd, nzpd=nzpd, meanint=meanint,
                 sigmaint=sigmaint, sigmat=sigmat, s=s, r=r)

datalist_nt <- list(ns=ns, y=y, time=time, np=np, ng=ng, wpo=wpo, vpo=vpo,
                    upo=upo, nzpo=nzpo, wpd=wpd, vpd=vpd, upd=upd, nzpd=nzpd, meanint=meanint,
                    sigmaint=sigmaint, s=s, r=r)


#### Run model and check leave-one-out PSID cross validation results ----------#

pm_mod <- readr::read_lines(here("model/shots/cv/ppool_cv.stan"))
pm_mod_nt <- readr::read_lines(here("model/shots/cv/ppool_nt_cv.stan"))

pm_fit <- chkpt_stan(model_code = pm_mod, 
                     data = datalist, 
                     iter_per_chkpt = 100,
                     parallel_chains = 4,
                     path = chkpt_folder)

pm_fit_nt <- chkpt_stan(model_code = pm_mod, 
                        data = datalist_nt, 
                        iter_per_chkpt = 100,
                        parallel_chains = 4,
                        path = chkpt_folder)


(loo_fit <- loo(pm_fit))
(loo_fit_nt <- loo(pm_fit_nt))

loo_compare(loo_fit, loo_fit_nt)


## Saving models ----------------------------------------------------------####

#draws_cv <- combine_chkpt_draws(object = pm_fit)

#draws_cv_nt <- combine_chkpt_draws(object = pm_fit_nt)

#output_dir <- "model/shots/cv/output"

#model_fname <- glue::glue("cv-ppool_{outcome}_{seasons}_{lubridate::today()}.rds")
#model_fname_nt <- glue::glue("cv-ppool_{outcome}_{seasons}_nt_{lubridate::today()}.rds")

#saveRDS(draws_cv, here(output_dir, model_fname))
#saveRDS(draws_cv_nt, here(output_dir, model_fname_nt))











