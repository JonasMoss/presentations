data {
  int<lower = 0> N_MATCHES;          // Number of matches.
  int<lower = 0> N_TEAMS;            // Number of teams.
  int<lower = 0> N_CLUSTERS;         // Maximal number of clusters.
  int<lower = 0> N_CLUSTERS_TOP;     // Maximal number of clusters at top level.
  int<lower = 0> N_MATCHES_PER_TEAM; // Maximal number of matches per team.
  int y[N_MATCHES, 4];               // Data.
}

parameters {
  real baseline;
  real homefield;
  
  real<lower = 0, upper = 1> betas[N_TEAMS, N_CLUSTERS]; 
  real<lower = 0, upper = 1> betas_top[N_CLUSTERS];
  
  real attack_parameters[N_MATCHES];
  real defense_parameters[N_MATCHES];
  
  real attack[N_CLUSTERS_TOP, N_CLUSTERS];
  real defense[N_CLUSTERS_TOP, N_CLUSTERS];
  int<lower = 1> centers[N_TEAMS]; 
    
  real alpha;
  real alpha_top;
}

transformed parameters{
  simplex[N_CLUSTERS] sticks[N_CLUSTERS_TOP];
  simplex[N_CLUSTERS_TOP] sticks_top;
  
  sticks_top[1] = sticks_top[1];
  for(j in 2:(N_CLUSTERS_TOP - 1)) {
    sticks_top[j] = betas_top[j] / betas_top[j-1] * 
                    (1 - betas_top[j-1]) * sticks_top[j-1]; 
  }
  sticks_top[N_CLUSTERS_TOP] = 1 - sum(sticks_top[1:(N_CLUSTERS_TOP-1)]);
  
  for(i in 1:N_TEAMS) {
    sticks[1, i] = sticks[1, i];
    for(j in 2:(N_CLUSTERS - 1)) {
      sticks[j, i] = betas[j, i] / betas[j-1, i] * 
                     (1 - betas[j-1, i]) * sticks[j-1, i]; 
    }
    sticks[N_CLUSTERS, i] = 1 - sum(sticks[1:(N_CLUSTERS-1), i]);
  }
  
  
}

model {
  real lambda_home;
  real lambda_away;
  real home_attack;
  real home_defense;
  real away_attack;
  real away_defense;
  int match_number[N_TEAMS];
  for(i in 1:N_TEAMS) match_number[i] = 0;
  
  // ===========================================================================
  // Prior.
  // ===========================================================================
  baseline ~ normal(0, 1);
  homefield ~ normal(0, 1);
  
  for(i in 1:N_TEAMS) for(j in 1:N_CLUSTERS) betas[i, j] ~ beta(1, alpha);
  betas_top ~ beta(1, alpha_top);
  
  // The probability measure 'H'. Given the number of clusters at both levels,
  // we only need to sample observations from the correct H. 
  for(i in N_CLUSTERS_TOP) {
    for(j in N_CLUSTERS) {
      attack[i, j]  ~ normal(0, 1)
      defense[i, j] ~ normal(0, 1)
    }
  }
  
  // The probability measures 'P_j'. These are represented as indicators. 
  for(i in 1:N_TEAMS) centers[i] ~ multinomial(sticks_top);
  
  // Concentration parameters.
  alpha ~ exponential(1);
  alpha_top ~ exponential(1);

  // ===========================================================================
  // Likelihood.
  // ===========================================================================
  
  for(i in 1:N_MATCHES) {
    int home_index = y[i, 3];
    int away_index = y[i, 4];
    
    for(j in 1:N_CLUSTERS){
      for(k in 1:N_CLUSTERS_TOP) {
        attack_parameters[i] ~ multinomial()
        defense_parameters[i];
      }
    }
    
    
    for(j in 1:N_CLUSTERS){
      
      for(k in 1:N_CLUSTERS_TOP) {
        
        home_attack  = attack[k, j];
        home_defense = defense[k, j];
        away_attack  = attack[k, j];
        away_defense = defense[k, j];
      
        lambda_home = baseline + homefield + home_attack - away_defense
        lambda_away = baseline + away_attack - home_defense
      
        target += log_sum_exp(log(sticks_top[k]) + log(sticks[k, j]) + 
                  poisson_log_lpmf(y[i, 1] | lambda_home);
        target += log_sum_exp(log(sticks_top[k]) + log(sticks[k, j]) + 
                  poisson_log_lpmf(y[i, 2] | lambda_away);      
    }
    
  }
  
  
  
}

