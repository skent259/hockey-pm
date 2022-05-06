library(here)
library(glue)
library(rstan)
library(Matrix)
source(here("analysis/utils.R"))

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

## Command line arguments -----------------------------------------------------#
#' @argument `outcome` Outcome variable to use, options are 'mi-bl' and 'sh-go'
#' @argument `d_fname` name of the data set to use 
args = commandArgs(trailingOnly = TRUE)
print(args)

outcome <- args[1]
d_fname <- as.integer(args[2]) + 1

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if(is.na(.x)) val else .x 
}
outcome <- set_default(outcome, "sh-go")
d_fname <- set_default(d_fname, "sog-model-data_o-sh-go_s'21_2022-04-25.rds")


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

## Run model and save ---------------------------------------------------------#

pm_mod <- stan_model(file = here("model", "shots", "ppool.stan"))
pm_fit <- sampling(object = pm_mod, 
                   data = datalist)

seasons <- pull_seasons(d_fname)
model_fname <- glue::glue("ppool_{outcome}_{seasons}_{lubridate::today()}.rds")
saveRDS(pm_fit, here("model", "shots", "output", model_fname))

