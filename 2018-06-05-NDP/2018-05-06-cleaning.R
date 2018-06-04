library("tidyverse")

season1 = readr::read_csv("2018-06-05-NDP/E0.csv")
season1$Season = 2016
season2 = readr::read_csv("2018-06-05-NDP/E0(1).csv")
season2$Season = 2017

season = rbind(season1, season2)
readr::write_csv(season, "PL.csv")

season = readr::read_csv("PL.csv")

season %>% 
  select(HomeTeam, AwayTeam, Season, FTHG, FTAG) %>%
  mutate(HomeTeam = as.factor(HomeTeam),
         AwayTeam = as.factor(AwayTeam),
         HomeInt  = as.integer(as.numeric(HomeTeam, levels = HomeTeam)),
         AwayInt  = as.integer(as.numeric(AwayTeam, levels = AwayTeam))) %>%
  rename(HomeGoals = FTHG,
         AwayGoals = FTAG) ->
  cleaned

data = cleaned[, 4:7]

library("rstan")
rstan_options(auto_write = TRUE)

simple_poisson = rstan::stan_model(file = "2018-06-05-NDP/simple_poisson.stan",
                                   model_name = "simple_poisson")

rstan::sampling(simple_poisson, data = list(N_TEAMS = max(data$AwayInt),
                                            N_MATCHES = nrow(data),
                                            y = data))



hierarchical_poisson = rstan::stan_model(file = "2018-06-05-NDP/hierarchical_poisson.stan",
                                         model_name = "hierarchical_poisson")


rstan::sampling(hierarchical_poisson, data = list(N_TEAMS = max(data$AwayInt),
                                                  N_MATCHES = nrow(data),
                                                  N_MATCHES_PER_TEAM = 2*38,
                                                  y = data),
                chains = 1) -> hierarchical



ndp_poisson = rstan::stan_model(file = "2018-06-05-NDP/ndp_poisson.stan",
                                         model_name = "ndp_poisson")


rstan::sampling(unidentified_poisson, data = list(N_TEAMS = max(data$AwayInt),
                                                  N_MATCHES = nrow(data),
                                                  N_CLUSTERS = 5,
                                                  N_MATCHES_PER_TEAM = 2*38,
                                                  y = data),
                chains = 1) -> unid

levels(cleaned$HomeTeam)
attack = setNames(colMeans(rstan::extract(hierarchical)$attack_mean), levels(cleaned$HomeTeam))
defense = setNames(colMeans(rstan::extract(hierarchical)$defense_mean), levels(cleaned$HomeTeam))
homefield = setNames(colMeans(rstan::extract(hierarchical)$homefield), levels(cleaned$HomeTeam))

hist(rstan::extract(unid)$attack[, 12, ])

hist(rstan::extract(unid)$attack[, 13, ])

quant10 = apply(rstan::extract(hierarchical)$homefield, 2, function(x) quantile(x, c(0.1)))
median = apply(rstan::extract(hierarchical)$homefield, 2, function(x) quantile(x, c(0.5)))
quant90 = apply(rstan::extract(hierarchical)$homefield, 2, function(x) quantile(x, c(0.9)))
plot(median, ylim = c(0, 0.6))
lines(quant10)
lines(quant90)

## =============================================================================
##  Dynamic model
## =============================================================================

library("rstan")
dynamic_poisson = rstan::stan_model(file = "2018-06-05-NDP/dynamic_poisson.stan",
                                         model_name = "dynamic_poisson")


rstan::sampling(dynamic_poisson, data = list(N_TEAMS = max(data$AwayInt),
                                                  N_MATCHES = nrow(data),
                                                  N_MATCHES_PER_TEAM = 2*38,
                                                  y = data),
                chains = 1) -> dynamic2

season %>% 
  select(HomeTeam, AwayTeam, Season, FTHG, FTAG, Date) %>%
  mutate(HomeTeam = as.factor(HomeTeam),
         AwayTeam = as.factor(AwayTeam),
         HomeInt  = as.integer(as.numeric(HomeTeam, levels = HomeTeam)),
         AwayInt  = as.integer(as.numeric(AwayTeam, levels = AwayTeam))) %>%
  rename(HomeGoals = FTHG,
         AwayGoals = FTAG) %>%
  mutate(Date = lubridate::decimal_date(lubridate::as_date(Date)))->
  cleaned



cleaned %>% 
  group_by(HomeTeam) %>%
  summarise(n = n()) %>%
  pull(n) ->
  games_played_home

cleaned %>% 
  group_by(AwayTeam) %>%
  summarise(n = n()) %>%
  pull(n) ->
  games_played_away

games_played = setNames(games_played_home + games_played_away, levels(cleaned$HomeTeam))
names(games_played) = sapply(names(games_played), function(str) stringr::str_replace_all(str, " ", "_"))
string = lapply(1:length(games_played), function(i) paste0("real ", names(games_played)[i], "[", games_played[i], "];\n"))
write(do.call(paste0, string), "hei.txt")

