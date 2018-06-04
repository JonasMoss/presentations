data {
  int<lower = 0> N_MATCHES;          // Number of matches.
  int<lower = 0> N_TEAMS;            // Number of teams.
  int<lower = 0> N_MATCHES_PER_TEAM; // Maximal number of matches per team.
  int y[N_MATCHES, 4];               // Data.
  
}

parameters {
  real attack[N_TEAMS, N_MATCHES_PER_TEAM]; 
  real attack_base;
  real<lower = 0, upper = 0.01> attack_sigma[N_TEAMS];
  
  real defense[N_TEAMS, N_MATCHES_PER_TEAM];
  real defense_base;
  real<lower = 0, upper = 0.01> defense_sigma[N_TEAMS];

  real homefield;
}

model {
  // Some declarations that must be here.
  real baseline = 1;
  real home_attack;
  real home_defense;
  real away_attack;
  real away_defense;
  
  // This array is used to keep track of the match number for each team. This is
  // need since we estimate a parameter for each match.
  int match_number[N_TEAMS];
  for(i in 1:N_TEAMS) match_number[i] = 0;
  
  // ===========================================================================
  // Prior.
  // ===========================================================================

  homefield ~ exponential(1);
  attack_base ~ normal(0, 1);
  defense_base ~ normal(0, 1);
  
  for(i in 1:N_TEAMS) {
    attack_sigma[i] ~ normal(0, 1) T[0, 0.01];
    defense_sigma[i] ~ normal(0, 1) T[0, 0.01];
  }
  
  // ===========================================================================
  // Likelihood.
  // ===========================================================================
    
    for(i in 1:N_MATCHES) {
      int home_index = y[i, 3];
      int away_index = y[i, 4];
      
      match_number[home_index] += 1;
      match_number[away_index] += 1;
      
      if(match_number[home_index] == 1) {
        attack[home_index, 1] ~ normal(attack_base, 1);
        defense[home_index, 1] ~ normal(defense_base, 1);
      } else {
        attack[home_index, match_number[home_index]] ~ 
          normal(attack[home_index, match_number[home_index] - 1], attack_sigma[home_index]);
        defense[home_index, match_number[home_index]] ~ 
          normal(defense[home_index, match_number[home_index] - 1], defense_sigma[home_index]);
      }
      
      if(match_number[away_index] == 1) {
        attack[away_index, 1] ~ normal(attack_base, 1);
        defense[away_index, 1] ~ normal(defense_base, 1);
      } else {
        attack[away_index, match_number[away_index]] ~ 
          normal(attack[away_index, match_number[away_index] - 1], attack_sigma[away_index]);
        defense[away_index, match_number[away_index]] ~ 
          normal(defense[away_index, match_number[away_index] - 1], defense_sigma[away_index]);
      }
      
      home_attack = attack[home_index, match_number[home_index]];      
      home_defense = defense[home_index, match_number[home_index]];        
      away_attack = attack[away_index, match_number[away_index]];      
      away_defense = defense[away_index, match_number[away_index]]; 
      
      y[i, 1] ~ poisson_log(baseline + homefield + home_attack - away_defense);
      y[i, 2] ~ poisson_log(baseline + away_attack - home_defense);
    }
}
