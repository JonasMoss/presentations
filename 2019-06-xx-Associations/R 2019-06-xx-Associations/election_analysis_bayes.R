library("tidyverse")
library("magrittr")
library("rstan")

election_analysis = stan_model(file = "election_analysis_bayes.stan", 
                               model_name = "election_analysis")

election = readRDS(file = "election.rds")

county_index = as.integer(as.factor(election$county_id))
year_index = as.integer(as.factor(election$year))
state_index = data.frame(county = county_index, 
                         state = as.integer(as.factor(election$state))) %>%
  group_by(county) %>% 
  summarize(state = min(state)) %>%
  pull(state)
states = unique(election$state)


x = matrix(0, nrow = max(county_index), ncol = max(year_index))

for(i in 1:nrow(election)) {
  x[county_index[i], year_index[i]] = election$white[i]
}

election_analysis_data = list(
  "I" = max(county_index),
  "J" = max(state_index),
  "T" = max(year_index),
  "N" = length(county_index),
  "y" = election$democrat_percent,
  "county_index" = county_index,
  "year_index" = year_index,
  "state_index" = state_index,
  "x" = x
)

samples = sampling(object = election_analysis, 
                   data = election_analysis_data,
                   chains = 1,
                   iter = 1000)
