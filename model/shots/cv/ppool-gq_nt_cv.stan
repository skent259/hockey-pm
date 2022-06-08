// Shots model (Poisson regression) with cross-validation

data {
  int<lower = 0> ns; // Number of shifts
  int<lower = 0> y[2*ns]; // Vector with shot counts 
  vector[2*ns] time; //Vector of shift durations
  
  int<lower =0> np; //Number of non-goalie players
  int<lower =0> ng; //Number of goalies
  
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
  real<lower = 0> s; //Shape for inverse gamma distribution of player effect variance
  real<lower = 0> r; //Rate for inverse gamma distribution of player effect variance
  
}

transformed data{
  vector[2*ns] logtime;
  logtime = log(time);
  
}

parameters {
  real mu; //intercept in regression
  vector[np] beta_off; //player offensive effects
  vector[np+ng] beta_def; //player defensive effects
  real<lower = 0> sigmap_squared; //variance of player effects
}

transformed parameters {
  real<lower=0> sigmap = sqrt(sigmap_squared);
}

generated quantities {
  real test_pred[2*ns];
  {
    // Variables in nested block are local, don't get output: 
    // see https://mc-stan.org/docs/2_29/reference-manual/program-block-generated-quantities.html
    vector[2*ns] logtime_gq;
    vector[2*ns] player_off; 
    vector[2*ns] player_def;
    vector[2*ns] log_lambda;
    
    player_off = csr_matrix_times_vector(2*ns, np, wpo, vpo, upo, beta_off); // XPO %*% beta_off
    player_def = csr_matrix_times_vector(2*ns, np+ng, wpd, vpd, upd, beta_def); // XPD %*% beta_def
    
    log_lambda = mu + player_off + player_def + logtime;
    
    for (i in 1:(2*ns)) {
      test_pred[i] = poisson_rng(exp(log_lambda[i]));
    }  
  }
}

