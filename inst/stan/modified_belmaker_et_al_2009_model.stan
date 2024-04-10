data{
  int <lower = 1> N;  // number of rows in the data
  int <lower = 1> native_total; // assumed size of native pool
  array[N] int <lower = 0> dI; // observed number of yearly records invasive
  array[N] int <lower = 0> dN; // observed number of yearly records native
  vector <lower = 0>[N] t; // time from start
  real b0_mu; // prior for betas
  real b1_mu; // prior for betas
  real b0_sd; // prior for betas
  real b1_sd; // prior for betas
}

transformed data {
  array[N] int <lower = 0> unrecorded_N;
  array[N] int <lower = 0> yearly_detections;
  array[N] int <lower = 0> recorded_I;  // cumulative recorded invasives
  array[N] int <lower = 0> recorded_N; // cumulative recorded natives

  recorded_I = cumulative_sum(dI);
  recorded_N = cumulative_sum(dN);

  for (i in 1:N){
    unrecorded_N[i] = native_total - recorded_N[i];
    yearly_detections[i] = dI[i] + dN[i];
  }

}

parameters {
  real b0;
  real b1;
}

transformed parameters {
  vector<lower = 0>[N] mu_t;
  vector<lower = 0>[N] unrecorded_I;

  mu_t = exp(b0 + b1 .* t);

  for (i in 1:N){
    unrecorded_I[i] = cumulative_sum(mu_t)[i] - recorded_I[i];
  }

}

model{

  dI ~ beta_binomial(yearly_detections, unrecorded_I, unrecorded_N);

  //priors
  b0  ~ normal(b0_mu, b0_sd);
  b1  ~ normal(b1_mu , b1_sd);
}

generated quantities {
  array[N] int y = beta_binomial_rng(yearly_detections, unrecorded_I, unrecorded_N);
}
