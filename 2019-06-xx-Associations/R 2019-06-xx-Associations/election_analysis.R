election = readRDS(file = "election.rds")
election$year = as.factor(election$year)

glm(democrat_won ~ white + state, 
    family = binomial, 
    data = election)

glm(democrat_won ~ white + year*state, 
    family = binomial, 
    data = election) -> mod

glm(democrat_won ~ white + state + year, 
    family = binomial, 
    data = election)

glm(democrat_won ~ white + year, 
    family = binomial, 
    data = election)

glm(democrat_won ~ year, 
    family = binomial, 
    data = election)

glm(democrat_won ~ white, 
    family = binomial, 
    data = election)

glm(democrat_won ~ state, 
    family = binomial, 
    data = election)

betareg::betareg(democrat_percent ~ white + state, 
                 data = election) %>%
    AIC

betareg::betareg(democrat_percent ~ state, 
                 data = election) %>%
    AIC

betareg::betareg(democrat_percent ~ white, 
                 data = election) %>%
    AIC

model = lme4::lmer(democrat_percent ~ 1 + year + white + (1 | county_id/state), 
                   data = election)

model2 = lme4::lmer(democrat_percent ~ 1 + year + white + (1 | state), 
                   data = election)

plot(x = 2000 + c(0, 4, 8, 12, 16),
     dplyr::filter(z, county == "Autauga")$per_democrat)
points(dplyr::filter(z, state == "North Dakota")$per_democrat, col = "green", pch = 20)

values = colMeans(extract(samples)$zeta)
names(values) = unique(election$state)

hist(log(election$democrat_percent))

mod = lm(democrat_percent ~ 1 + year + white + county_id, 
           data = election)
