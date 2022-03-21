library(here)
library(glue)
library(rstan)
library(Matrix)
source(here("analysis/utils.R"))

d_fname <- "model-data_s'21_off-def_2022-03-20.rds"
d <- readRDS(here("data", d_fname))

## Set up data list for Stan --------------------------------------------------#

X <- matrix(d, nrow=dim(d)[1]) #Convert to dense type
ng <- nrow(X) #Number of goals 
y <- ifelse(X[, 1] == -1, 0, 1)
nt <- 31
np <- (ncol(X) - 32) / 2
XT <- X[, 2:32]
XPO <- X[, 32 + 1:np]
XPD <- X[, 32 + np + 1:np]
nzt <- 2*ng
nzp <- 6*ng
sigmat <- 1
s <-  7.5
r <- 0.5

datalist <- list(ng=ng, y=y, nt=nt, np=np, XT=XT, XPO=XPO, XPD=XPD, nzt=nzt,
                 nzp=nzp, sigmat=sigmat, s=s, r=r)

## Model ----------------------------------------------------------------------#

# FROM SECTION 3 OF GRAMACY PAPER: We have set σt  = 1 and r = 1/2 throughout Section 3. 
# In choosing s = E[λj]/2,
# we focus on the conditional prior standard deviation, SD(βj ) = √2/λj, for the
# coefficients. Hence our value of s = 7.5, for E[λj] = 15, 
# implies expected SD(βj ) ≈ 0.095.
# To put this in context, exp[3 × 0.095] ≈ 1.33, implying that a single player
# increasing his team’s for-vs-against odds by 1/3 is 3 deviations away from the
# prior mean.


# In our model, since we have βj ~ N(0, 1/λ) instead of βj ~ Laplace(0,1/λ), the
# conditional prior standard deviation is SD(βj)=√(1/λ) and so expected 
# SD(βj) = √(1/15) = 0.258.

# Notice also that we have a single, global lambda parameter, not a separate one
# for each player, since we are partially pooling. I think this is what the
# authors do in Section 4 of their paper, their full posterior analysis.


pm_mod <- stan_model(file = here("model", "ppool_off-def.stan"))
pm_fit <- sampling(object = pm_mod, 
                   data = datalist)

seasons <- pull_seasons(d_fname)
model_fname <- glue::glue("ppool_off-def_{seasons}_{lubridate::today()}.rds")
saveRDS(pm_fit, here("model", model_fname))
# save('pm_fit', file = here("model", model_fname))
     
     
## Analysis -------------------------------------------------------------------#

team_names <- colnames(d)[2:32]
player_names <- colnames(d)[32 + 1:np]

plot_post_parameter(pm_fit, )

plot_post_parameter(pm_fit, vars(starts_with("beta_off")), player_names) +
  theme(axis.text.y = element_blank()) 

plot_post_parameter(pm_fit, vars(starts_with("beta_def")), player_names) +
  theme(axis.text.y = element_blank()) 

## Diagnostics ----------------------------------------------------------------#

plot_rhat_multi(pm_fit, par_list = c("lambda", "alfa", "beta_off", "beta_def")) +
  patchwork::plot_annotation(title = "Rhat plot")







