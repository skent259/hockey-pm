library(here)
library(glue)
library(rstan)
library(Matrix)
source(here("analysis/utils.R"))

d_fname <- "sog-model-data_o-mi-bl_s'18'19'20'21_2022-04-21.rds"
d <- readRDS(here("data", d_fname))

## Set up data list for Stan --------------------------------------------------#

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

meanint = -5.5 #Based on simulation
sigmaint = 1
sigmat <- .5
s <-  7.5
r <- 0.5

datalist <- list(ns=ns, y=y, time=time, nt=nt, np=np, ng=ng, wto=wto, vto=vto, 
                 uto=uto, nzt=nzt, wtd = wtd, vtd=vtd, utd=utd, wpo=wpo, vpo=vpo,
                 upo=upo, nzpo=nzpo, wpd=wpd, vpd=vpd, upd=upd, nzpd=nzpd, meanint=meanint,
                 sigmaint=sigmaint, sigmat=sigmat, s=s, r=r)



pm_mod <- stan_model(file = here("model", "shots", "ppool.stan"))
pm_fit <- sampling(object = pm_mod, 
                   data = datalist)

seasons <- pull_seasons(d_fname)
model_fname <- glue::glue("ppool_mi-bl_{seasons}_{lubridate::today()}.rds")
saveRDS(pm_fit, here("model", "shots", model_fname))



