// Shots model (Poisson regression) 

data {
  int<lower = 0> ns; // Number of shifts
  int<lower = 0> y[2*ns]; // Vector with shot counts 
  vector[2*ns] time; //Vector of shift durations
  
  int<lower = 0> nt; //Number of teams
  int<lower =0> np; //Number of non-goalie players
  int<lower =0> ng; //Number of goalies
  
  //Sparse representation of offensive team design matrix
  int<lower = 0> nzt; // length 
  vector[nzt] wto; 
  int vto[nzt];  
  int uto[2*ns+1]; 
  
  //Sparse representation of defensive team design matrix
  vector[nzt] wtd; 
  int vtd[nzt];  
  int utd[2*ns+1]; 
  
  //Sparse representation of Offensive Player Design matrix
  int<lower = 0> nzpo; //length
  vector[nzpo] wpo;
  int vpo[nzpo];
  int upo[2*ns+1];
  
  //Sparse representation of defensive player design matrix
  int<lower = 0> nzpd; //length
  vector[nzpd] wpd;
  int vpd[nzpd];
  int upd[2*ns+1];
  
  real meanint; //Intercept prior mean
  real<lower = 0> sigmaint; //Intercept prior variance
  real<lower = 0> sigmat; //Team effect prior variance
  real<lower = 0> s; //Shape for inverse gamma distribution of player effect variance
  real<lower = 0> r; //Rate for inverse gamma distribution of player effect variance
  
}


transformed data{
  vector[2*ns] logtime;
  logtime = log(time);
  
}

parameters {
  real mu; //intercept in regression
  vector[nt] alfa_off; //team offensive effects
  vector[nt] alfa_def; //team defensive effects
  vector[np] beta_off; //player offensive effects
  vector[np+ng] beta_def; //player defensive effects
  real<lower = 0> sigmap_squared; //variance of player effects
}


transformed parameters {
  real<lower=0> sigmap = sqrt(sigmap_squared);
}


model {
  vector[2*ns] team_off;
  vector[2*ns] team_def;
  vector[2*ns] player_off; 
  vector[2*ns] player_def;
  vector[2*ns] mu_vector;
  vector[2*ns] log_lambda;
  
  //Layer 1
  sigmap_squared ~ inv_gamma(s, r);
  
  //Layer 2
  mu ~ normal(meanint, sigmaint);
  alfa_off ~ normal(0, sigmat);
  alfa_def ~ normal(0, sigmat);
  beta_off ~ normal(0, sigmap);
  beta_def ~ normal(0, sigmap);
  
  //Layer 3
  //Perform sparse matrix-vector multiplication: see https://mc-stan.org/docs/2_29/functions-reference/sparse-matrix-arithmetic.html
  
  team_off = csr_matrix_times_vector(2*ns, nt, wto, vto, uto, alfa_off); // XTO %*% alpha
  team_def = csr_matrix_times_vector(2*ns, nt, wtd, vtd, utd, alfa_def); // XTD %*% alpha
  player_off = csr_matrix_times_vector(2*ns, np, wpo, vpo, upo, beta_off); // XPO %*% beta_off
  player_def = csr_matrix_times_vector(2*ns, np+ng, wpd, vpd, upd, beta_def); // XPD %*% beta_def
  //Broadcast mu (intercept parameter) into vec
  mu_vector = rep_vector(mu, 2*ns); 
  
  log_lambda = mu_vector + team_off + team_def + player_off + player_def + logtime;
  y ~ poisson_log(log_lambda);
  
}

generated quantities {
  vector[2*ns] logtime_gq;
  vector[2*ns] team_off;
  vector[2*ns] team_def;
  vector[2*ns] player_off; 
  vector[2*ns] player_def;
  vector[2*ns] log_lambda;
  real test_pred[2*ns];
  
  logtime_gq = log(time);
  team_off = csr_matrix_times_vector(2*ns, nt, wto, vto, uto, alfa_off); // XTO %*% alpha
  team_def = csr_matrix_times_vector(2*ns, nt, wtd, vtd, utd, alfa_def); // XTD %*% alpha
  player_off = csr_matrix_times_vector(2*ns, np, wpo, vpo, upo, beta_off); // XPO %*% beta_off
  player_def = csr_matrix_times_vector(2*ns, np+ng, wpd, vpd, upd, beta_def); // XPD %*% beta_def
  
  log_lambda = mu + team_off + team_def + player_off + player_def + logtime_gq;
  
  for (i in 1:(2*ns)) {
    test_pred[i] = poisson_rng(exp(log_lambda[i]));
  }
  
}



