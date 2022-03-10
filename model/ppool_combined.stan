//

data {
  int<lower = 0> ng; // number of goals
  int<lower = 0, upper = 1> y[ng]; // 0-1 outcomes
  int<lower = 0> nt; //number of teams
  int<lower =0> np; //number of players
  matrix<lower=-1, upper=1>[ng, nt] XT; //team design matrix
  matrix<lower=-1, upper=1>[ng, np] XP; //player design matrix
  int<lower = 0> nzt; //number of non-zero entries in team design matrix
  int<lower = 0> nzp; //Number of non-zero entries in player design matrix
  
  real<lower = 0> sigmat; //Team effect prior variance
  real<lower = 0> s; //Shape for gamma distribution of global hyperparameter lambda
  real<lower = 0> r; //Rate for gamma distribution of global hyperparameter lambda
  
}

transformed data{
  //Obtain sparse representation of team and player design matrices: see https://mc-stan.org/docs/2_29/functions-reference/CSR.html
  //Declaration
  vector[nzt] wt; 
  int vt[nzt];  
  int ut[ng+1]; 
  vector[nzp] wp;
  int vp[nzp];
  int up[ng+1];
  
  //Assignments: see https://mc-stan.org/docs/2_29/functions-reference/conversion-functions.html
  wt = csr_extract_w(XT);
  vt = csr_extract_v(XT);
  ut = csr_extract_u(XT);
  wp = csr_extract_w(XP);
  vp = csr_extract_v(XP);
  up = csr_extract_u(XP);
  
}


parameters {
  vector[nt] alfa; //team effects
  vector[np] beta; //player effects
  real<lower = 0> lambda; //precision for distribution of player effects 
}


transformed parameters {
  real<lower = 0> sigmap; //variance for distribution of player effects
  sigmap = 1/lambda; 
}


model {
  
  //Perform sparse matrix-vector multiplication: see https://mc-stan.org/docs/2_29/functions-reference/sparse-matrix-arithmetic.html
  vector[ng] team; // XT %*% alpha
  vector[ng] player; // XP %*% beta
  team = csr_matrix_times_vector(ng, nt, wt, vt, ut, alfa);
  player = csr_matrix_times_vector(ng, np, wp, vp, up, beta);
  
  lambda ~ gamma(s, r);
  alfa ~ normal(0, sigmat);
  beta ~ normal(0, sigmap);
  y ~ bernoulli_logit(team+player);
  
  
}

