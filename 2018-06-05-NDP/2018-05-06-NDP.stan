data {
  int<lower = 0> C; //num of clusters
  int<lower = 0> N; //data num
  real y[N];
}

parameters {
  real mu_cl[C]; //cluster mean
  real <lower=0, upper=1> v[C]; // for stick-breaking
  real <lower=0> sigma_cl[C]; // error scale
}

transformed parameters{
  simplex [C] pi;
  pi[1] = v[1];
  for(j in 2:(C-1)) pi[j]= v[j]*(1-v[j-1])*pi[j-1]/v[j-1]; 
  pi[C] = 1 - sum(pi[1:(C-1)]); // to make a simplex.
}

model {
  real alpha = 1;
  real ps[C];
  sigma_cl ~ exponential(1);
  mu_cl ~ normal(0, 1);
  v ~ beta(1, alpha);
  
  for(i in 1:N){
    for(c in 1:C){
      ps[c] = log(pi[c]) + normal_lpdf(y[i]|mu_cl[c],sigma_cl[c]);
    }
    target += log_sum_exp(ps);
  }

}