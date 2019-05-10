data {
  int N;      // observations
  int J;      // counties
  int y[N];   // sentences (per county-year)
  int start[J];
  int end[J];
  int<lower=1,upper=47> countyid[N];    // counties
  vector<lower=0>[N] wpop_meas;
  vector<lower=0>[N] bpop_meas;
  vector<lower=0>[N] wrate;
  vector<lower=0>[N] brate;
  real<lower=0> tau;
}
transformed data {
  vector<lower=0>[N] tau_bp;
  vector<lower=0>[N] tau_wp;
  int Y[J];
  for (n in 1:N) {
    tau_bp[n] = tau * bpop_meas[n];
  }
  for (n in 1:N) {
    tau_wp[n] = tau * wpop_meas[n];
  }
 for (j in 1:J) {
   Y[j] = sum(y[start[j]:end[j]]);
 }
}
parameters {
  real alpha;   // intercept
  vector[J] alpha_countyid; 
  real sd_cnty; // hyperprior on alpha_countyid 
  real<lower=0> wpop[N]; // unknown true population values
  real<lower=0> bpop[N];
  real mu_bpop[N];    // prior locations
  real mu_wpop[N];
  real sigma_wp;  // prior scales
  real sigma_bp;
}
transformed parameters {
   real<lower=0> E[N]; // exposure
   vector<lower=0>[J] log_E;
   vector[J] lambda;
   for (n in 1:N) {
     E[n] = wpop[n]*wrate[n] + bpop[n]*brate[n];
   }
   for (j in 1:J) {
  log_E[j] = fmax(log(sum(E[start[j]:end[j]])), 1); // now sum(E) across all years for each county 
   }
  for (j in 1:J)
    lambda[j] = alpha + alpha_countyid[j] + log_E[j];
   }
model { 
  bpop ~ normal(mu_bpop, sigma_bp);
  for (n in 1:N) {
   bpop_meas[n] ~ normal(bpop[n], tau_bp[n]) T[0,]; 
  }
  wpop ~ normal(mu_wpop, sigma_wp);
  for (n in 1:N) {
   wpop_meas[n] ~ normal(wpop[n], tau_wp[n]) T[0,]; 
  }
  Y ~ poisson_log(lambda);  
  alpha_countyid ~ normal(0, sd_cnty);
  sd_cnty ~ cauchy(0, 2) T[0,];
  alpha ~ student_t(3, 0, 10);
}
generated quantities {
  real y_pred[J]; 
  vector[J] SSR;
  for (j in 1:J) y_pred[j] = exp(lambda[j]);
  for (j in 1:J) SSR[j] = y_pred[j] / exp(log_E[j]);
}