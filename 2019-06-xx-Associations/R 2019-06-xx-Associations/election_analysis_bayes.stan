data {
  
  int I; // Number of counties.
  int J; // Number of states.
  int T; // Number of elections.
  int N; // Number of observations.
    
  real y[N]; // Observations. 
  int county_index[N];
  int year_index[N];
  int state_index[I];
  matrix[I, T] x; // Covariates.
}

parameters {
  real beta; 
  
  vector[I] alpha; 
  vector[J] zeta; 
  vector[T] gamma; 
  
  real<lower=0> sigma_y; //error sd
  real<lower=0> sigma_alpha; //subj sd
  real<lower=0> sigma_zeta; //item sd
}

model {
 // True priors.
 gamma ~ normal(0, 1);
 sigma_alpha ~ normal(0, 1) T[0,];
 sigma_zeta ~ normal(0, 1) T[0,];
 sigma_y ~ normal(0, 1) T[0,];
 
 
 // Multi-level structure.
 zeta ~ normal(0, sigma_zeta);
 for(i in 1:I) {
   alpha[i] ~ normal(zeta[state_index[i]], sigma_alpha);
 }
  
 // Likelihood.
 for(i in 1:(N)) {
    y[i] ~ normal(alpha[county_index[i]] + 
                  beta*x[county_index[i], year_index[i]] + 
                  gamma[year_index[i]], sigma_y);
 }

}
