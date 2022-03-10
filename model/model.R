library(here)
data <- readRDS(here("data", "model-data_s'18'19'20'21_combined_2022-03-10.rds"))


X <- matrix(data, nrow=dim(data)[1]) #Convert to dense type
ng <- nrow(X) #Number of goals 
y <- ifelse(X[,1] == -1, 0, 1)
nt <- 31
np <- ncol(X)-32
XT <- X[,2:32]
XP <- X[,33:ncol(X)]
nzt = 2*ng
nzp = 12*ng
sigmat=1
s = 7.5
r=.5


#FROM SECTION 3 OF GRAMACY PAPER: We have set σt  = 1 and r = 1/2 throughout Section 3. 
#In choosing s = E[λj]/2,
#we focus on the conditional prior standard deviation, SD(βj ) = √2/λj, for the
#coefficients. Hence our value of s = 7.5, for E[λj] = 15, 
#implies expected SD(βj ) ≈ 0.095.
#To put this in context, exp[3 × 0.095] ≈ 1.33, implying that a single player increasing his
#team’s for-vs-against odds by 1/3 is 3 deviations away from the prior mean.


# In our model, since we have βj ~ N(0, 1/λ) instead of βj ~ Laplace(0,1/λ),the conditional prior 
# standard deviation is SD(βj)=√(1/λ) and so expected SD(βj) = √(1/15) = 0.258.

#Notice also that we have a single, global lambda parameter, not a separate one for each player, 
#since we are partially pooling. I think this is what the authors do in Section 4 of their paper,
#their full posterior analysis.


datalist <- list(ng=ng, y=y, nt=nt, np=np, XT=XT, XP=XP, nzt=nzt, nzp=nzp, sigmat=sigmat, s=s, r=r)


pm_mod <- stan_model(file = here("modeling", "simple.stan"))
pm_fit <- sampling(object = pm_mod, 
                   data = datalist)


## Analysis #################


#Rank teams by posterior means
lambda_samp = rstan::extract(pm_fit, pars="lambda")[["lambda"]]
alfa_samp = rstan::extract(pm_fit, pars="alfa")[["alfa"]]
tpostmeans = colMeans(alfa_samp)
tnames = data@Dimnames[[2]][2:32]
trank = tnames[order(tpostmeans, decreasing=T)]


#Rank players by posterior means
beta_samp = rstan::extract(pm_fit, pars="beta")[["beta"]]
ppostmeans = colMeans(beta_samp)
pnames = data@Dimnames[[2]][33:ncol(data)]
prank = pnames[order(ppostmeans, decreasing=T)]
which(prank == "Sidney.Crosby")









