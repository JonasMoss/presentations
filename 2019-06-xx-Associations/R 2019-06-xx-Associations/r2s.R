r2 = function(alpha, beta, x, gamma, sigma) {
  1 - sigma^2/(sigma^2 + )
}

election$county_id = as.factor(election$county_id)
election$year = as.factor(election$year)
attr(election$county_id, "contrasts") = contrasts(election$county_id, contrasts = FALSE)
attr(election$year, "contrasts") = contrasts(election$year, contrasts = FALSE)

model.matrix(democrat_percent ~ 0 + white + as.factor(county_id) + as.factor(year),
             data = election) -> covariates

theta = c(mean(rstan::extract(samples)$beta),
          colMeans(rstan::extract(samples)$alpha),
          colMeans(rstan::extract(samples)$gamma))

covariates = scale(covariates, center = TRUE, scale = FALSE)

X = covariates%*%theta
c(crossprod(X, X))/nrow(X) -> b

b_state = setNames(sapply(unique(state_index), function(index) {
  p_state = sum(election$state == states[index])/nrow(election)
  alpha = colMeans(rstan::extract(samples)$alpha)[state_index == index]
  c(p_state*(crossprod(alpha, alpha)/length(alpha) - mean(alpha)^2))
}), unique(state_index))


sigma = mean(rstan::extract(samples)$sigma_y)

1 - (sigma^2 + sum(b_state))/(sigma^2 + b)
1 - (sigma^2)/(sigma^2 + b)
1 - (sigma^2)/(sigma^2 + sum(b_state))

1 - (sigma^2)/(sigma^2 + theta[1]^2*var(election$white))

(sigma^2 + b)/(sigma^2 + sum(b_state))
(sigma^2 + sum(b_state))/(sigma^2)
(sigma^2 + b)/(sigma^2)
(sigma^2 + theta[1]^2*var(election$white))/(sigma^2)
(sigma^2 + theta[1]^2*1/12)/(sigma^2)
