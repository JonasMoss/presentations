data {
  int<lower = 0> N_MATCHES;          // Number of matches.
  int<lower = 0> N_TEAMS;            // Number of teams.
  int<lower = 0> N_MATCHES_PER_TEAM; // Maximal number of matches per team.
  int y[N_MATCHES, 4];               // Data.
}

parameters {
  real attack[N_TEAMS, N_MATCHES_PER_TEAM];  // Attack per team per match.
  real attack_mean[N_TEAMS];
  real<lower = 0> attack_sd[N_TEAMS];  
  real attack_grand_mean;
  real<lower = 0> attack_grand_mean_sd;
  real<lower = 0> attack_grand_sd;  
  
  real defense[N_TEAMS, N_MATCHES_PER_TEAM]; // Defense per team per match.
  real defense_mean[N_TEAMS];
  real<lower = 0> defense_sd[N_TEAMS];
  real defense_grand_mean;
  real<lower = 0> defense_grand_mean_sd;
  real<lower = 0> defense_grand_sd;
  
  real baseline;
  
  real homefield[N_TEAMS];
  real homefield_mean;
  real<lower = 0> homefield_sd;
  
  // real awayfield[N_TEAMS];
  // real awayfield_mean;
  // real<lower = 0> awayfield_sd;
  
}

model {
  
  // Some declarations that must be here.
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
  
  baseline ~ normal(0, 1);
  
  homefield_mean ~ normal(0, 1);
  homefield_sd ~ exponential(1);
  homefield ~ normal(homefield_mean, homefield_sd);
  
  // awayfield_mean ~ normal(0, 1);
  // awayfield_sd ~ exponential(1);
  // awayfield ~ normal(awayfield_mean, awayfield_sd);
  
  attack_mean ~ normal(attack_grand_mean, attack_grand_mean_sd);
  defense_mean ~ normal(defense_grand_mean, defense_grand_mean_sd);
  attack_sd ~ exponential(1/attack_grand_sd);
  defense_sd ~ exponential(1/defense_grand_sd);
  
  attack_grand_mean ~ normal(0, 1);
  defense_grand_mean ~ normal(0, 1);
  attack_grand_mean_sd ~ exponential(1);
  defense_grand_mean_sd ~ exponential(1);
  
  attack_grand_sd ~ exponential(1);
  defense_grand_sd ~ exponential(1);
  
  for(i in 1:N_TEAMS) {
    attack[i] ~ normal(attack_mean[i], attack_sd[i]);
    defense[i] ~ normal(defense_mean[i], defense_sd[i]);    
    // for(j in 1:N_MATCHES_PER_TEAM) {
    //   attack[i, j] ~ normal(attack_mean[i], attack_sd[i]);
    //   defense[i, j] ~ normal(defense_mean[i], defense_sd[i]);      
    // }
  }

  // ===========================================================================
  // Likelihood.
  // ===========================================================================
  
  for(i in 1:N_MATCHES) {
    int home_index = y[i, 3];
    int away_index = y[i, 4];
    match_number[home_index] += 1;
    match_number[away_index] += 1;
    home_attack  = attack[home_index, match_number[home_index]];
    home_defense = defense[home_index, match_number[home_index]];
    away_attack  = attack[away_index, match_number[away_index]];
    away_defense = defense[away_index, match_number[away_index]];
    y[i, 1] ~ poisson_log(baseline + homefield[home_index] + home_attack - away_defense);
    y[i, 2] ~ poisson_log(baseline + away_attack - home_defense);
  }
}
