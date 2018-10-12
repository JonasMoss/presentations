data("dat.bangertdrowns2004", package = "metafor")



plot(x = 1/dat.bangertdrowns2004$vi,
     y = dat.bangertdrowns2004$yi,
     pch = 20,
     bty = "l",
     cex = 2,
     col = dat.bangertdrowns2004$meta + 1,
     xlab = "n", ylab = expression(theta),
     sub = paste0("n = ", nrow(dat.bangertdrowns2004)))
lines(x = 1/sort(dat.bangertdrowns2004$vi),
      y = 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(x = 1/sort(dat.bangertdrowns2004$vi),
      y = -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
abline(h = 0, lty = 3, col = "grey")
legend(x = "topright", col = c("black", "red", "black"), cex = 1.6, 
       pch = c(20, 20, NA), lty = c(NA, NA, 2),
       legend = c("Without meta-cognitive reflection",
                  "With meta-cognitive reflection",
                  expression(paste("1.96/", sqrt(n)))), bty = "n")

metafor::rma(yi, vi, data = dat.bangertdrowns2004)
metafor::rma(yi, vi, data = dplyr::filter(dat.bangertdrowns2004, meta == 1))
metafor::rma(yi, vi, data = dplyr::filter(dat.bangertdrowns2004, meta == 0))
metafor::rma(yi, vi, mods = ~ meta, data = dat.bangertdrowns2004)



bangertdrowns2004 = with(dat.bangertdrowns2004, {
  data.frame(z = 1/sqrt(vi)*yi,
             M = 1/vi,
             lower = qt(0.975, df = 1/vi),
             upper = qt(0.025, df = 1/vi),
             dist_indices = rep(5, length(yi))
             )
})

bangertdrowns2004$n = as.vector(scale(bangertdrowns2004$M))
bangertdrowns2004$sqrtn = as.vector(scale(sqrt(bangertdrowns2004$M)))
bangertdrowns2004$logn = as.vector(scale(log(bangertdrowns2004$M)))
bangertdrowns2004$meta = dat.bangertdrowns2004$meta
bangertdrowns2004$meta[is.na(bangertdrowns2004$meta)] = 0
bangertdrowns2004$meta = bangertdrowns2004$meta

bangertdrowns2004$dist_indices[bangertdrowns2004$z < qnorm(0.975)] = 10
bangertdrowns2004$dist_indices[-bangertdrowns2004$z > qnorm(0.975)] = 6
bangertdrowns2004_npb = bangertdrowns2004
bangertdrowns2004_npb$dist_indices = 20


straussR(formula = z ~ normal(mean ~ 1,
                              sd ~ 1,
                              p ~ 1),
         
         data = dplyr::filter(bangertdrowns2004, meta == 1),
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_mixed_pb_red


straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1,
                              p ~ 1),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_mixed_pb

straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1,
                              logit(p) ~ 1 + n),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ normal(1, 1),
                                   n ~ normal(0, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_mixed_pb_n

straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1,
                              logit(p) ~ 1 + sqrtn),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ normal(1, 1),
                                   sqrtn ~ normal(0, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_mixed_pb_sqrtn

straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1,
                              logit(p) ~ 1 + logn),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ normal(1, 1),
                                   logn ~ normal(0, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_mixed_pb_logn

straussR(formula = z ~ fixed(mean ~ 1,
                             logit(p) ~ 1 + logn),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       p    = list((Intercept) ~ normal(1, 1),
                                   logn ~ normal(0, 1))),
         
         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_fixed_pb_logn



straussR(formula = z ~ normal(mean ~ 1 + meta,
                              sd  ~ 1,
                              p ~ 1),

         data = bangertdrowns2004,

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   meta ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999,
                        max_treedepth = 15)) ->
  bangertdrowns2004_mixed_meta_pb

straussR(formula = z ~ normal(mean ~ 1 + meta,
                              sd  ~ 1,
                              logit(p) ~ 1 + logn),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   meta ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1)),
                       p    = list((Intercept) ~ normal(0, 1),
                                   logn ~ normal(0, 1))),
         
         chains = 2,
         control = list(adapt_delta = 0.9999,
                        max_treedepth = 17)) ->
  bangertdrowns2004_mixed_meta_pb_logn

straussR(formula = z ~ fixed(mean ~ 1 + meta,
                             p ~ 1),

         data = bangertdrowns2004,

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   meta ~ normal(0, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_fixed_meta_pb


straussR(formula = z ~ normal(mean ~ 1 + meta,
                              sd  ~ 1),

         data = bangertdrowns2004_npb,

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   meta ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_mixed_meta_npb

straussR(formula = z ~ fixed(mean ~ 1,
                             p ~ 1),

         data = bangertdrowns2004,

         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_fixed_pb

straussR(formula = z ~ fixed(mean ~ 1,
                             logit(p) ~ 1 + M),
         
         data = bangertdrowns2004,
         
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       p    = list((Intercept) ~ normal(0, 1),
                                   M ~ normal(0, 1))),
         
         chains = 1,
         control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_fixed_pb_n


straussR::straussR(formula = z ~ normal(mean ~ 1,
                                        sd  ~ 1),

                   data = bangertdrowns2004_npb,

                   priors = list(mean = list((Intercept) ~ normal(0, 1)),
                                 sd   = list((Intercept) ~ gamma(1, 1))),

                   chains = 4,
                   control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_mixed_npb

straussR::straussR(formula = z ~ fixed(mean ~ 1),

                   data = bangertdrowns2004_npb,

                   priors = list(mean = list((Intercept) ~ normal(0, 1))),

                   chains = 4,
                   control = list(adapt_delta = 0.999)) ->
  bangertdrowns2004_fixed_npb


bangertdrowns2004_mixed_pb$stan_object
bangertdrowns2004_mixed_npb$stan_object
bangertdrowns2004_fixed_pb$stan_object
bangertdrowns2004_fixed_npb$stan_object

plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = "black", pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
abline(h = 0.11, col = "blue", lty = 2, lwd = 2)
abline(h = 0.07, col = "black", lty = 2, lwd = 2)
abline(h = 0.21, col = "red", lty = 2, lwd = 2)

### load data
dat <- get(data(dat.bangertdrowns2004, package = "metafor"))

### random-effects model
res <- metafor::rma(yi, vi, data=dat)
res

set.seed(1337)
N = nrow(dat.bangertdrowns2004)
thetas = sample(rstan::extract(bangertdrowns2004_fixed_pb$stan_object)$beta_unbounded, N)
probs = sample(rstan::extract(bangertdrowns2004_fixed_pb$stan_object)$beta_unit, N)
hacked = rbinom(N, prob = probs, size = 1)
vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
       hacked*truncnorm::rtruncnorm(N,
                                    mean = thetas,
                                    sd = sqrt(dat.bangertdrowns2004$vi),
                                    a = 1.96*sqrt(dat.bangertdrowns2004$vi))


plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = "black", pch = 20)
points(1/dat.bangertdrowns2004$vi, vals, col = "blue", pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)


plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = "black", pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)


replicate(1000, {
N = nrow(dat.bangertdrowns2004)
thetas = sample(rstan::extract(bangertdrowns2004_fixed_pb$stan_object)$beta_unbounded, N)
probs = sample(rstan::extract(bangertdrowns2004_fixed_pb$stan_object)$beta_unit, N)
hacked = rbinom(N, prob = probs, size = 1)
vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
  hacked*truncnorm::rtruncnorm(N,
                               mean = thetas,
                               sd = sqrt(dat.bangertdrowns2004$vi),
                               a = 1.96*sqrt(dat.bangertdrowns2004$vi))
c(I2(vals, dat$vi), Q(vals, dat$vi), mean(ppred(vals, data = bangertdrowns2004)))
}) -> res_fixed

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb$stan_object)$beta_unbounded, N)
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_pb$stan_object)$beta_unit, N)
  hacked = rbinom(N, prob = probs, size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), Q(vals, dat$vi), mean(ppred(vals, data = bangertdrowns2004)))
}) -> res_mixed

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas = sample(rstan::extract(bangertdrowns2004_fixed_npb$stan_object)$beta_unbounded, N)
  vals = rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), Q(vals, dat$vi), mean(ppred(vals, data = bangertdrowns2004)))
}) -> res_fixed_npb

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_npb$stan_object)$beta_unbounded, N)
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_npb$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  vals = rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), Q(vals, dat$vi), mean(ppred(vals, data = bangertdrowns2004)))
}) -> res_mixed_npb


replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 1], N)
  thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 2], N)
  thetas0 = thetas0 + bangertdrowns2004$meta*thetas0_meta
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unit, N)
  hacked = rbinom(N, prob = probs, size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_mixed_meta_bp

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 1], N)
  thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 2], N)
  thetas0 = thetas0 + bangertdrowns2004$meta*thetas0_meta
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  vals = rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_mixed_meta_npb


replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_n$stan_object)$beta_unbounded[, 1], N)
  thetas0 = thetas0
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_n$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_pb_n$stan_object)$beta_unbounded[, 2], N)
  probs_n = sample(rstan::extract(bangertdrowns2004_mixed_pb_n$stan_object)$beta_unbounded[, 3], N)
  probs = probs + probs_n*bangertdrowns2004$n
  hacked = rbinom(N, prob = 1/(1 + exp(-probs)), size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), Q(vals, dat$vi), mean(ppred(vals, data = bangertdrowns2004)))
}) -> res_mixed_meta_pb_n

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_sqrtn$stan_object)$beta_unbounded[, 1], N)
  thetas0 = thetas0
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_sqrtn$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_pb_sqrtn$stan_object)$beta_unbounded[, 2], N)
  probs_n = sample(rstan::extract(bangertdrowns2004_mixed_pb_sqrtn$stan_object)$beta_unbounded[, 3], N)
  probs = probs + probs_n*bangertdrowns2004$sqrtn
  hacked = rbinom(N, prob = 1/(1 + exp(-probs)), size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_mixed_meta_pb_sqrtn

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 1], N)
  thetas0 = thetas0
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 2], N)
  probs_n = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 3], N)
  probs = probs + probs_n*bangertdrowns2004$logn
  hacked = rbinom(N, prob = 1/(1 + exp(-probs)), size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_mixed_pb_logn

replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 1], N)
  probs = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 2], N)
  probs_n = sample(rstan::extract(bangertdrowns2004_mixed_pb_logn$stan_object)$beta_unbounded[, 3], N)
  probs = probs + probs_n*bangertdrowns2004$logn
  hacked = rbinom(N, prob = 1/(1 + exp(-probs)), size = 1)
  vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
    hacked*truncnorm::rtruncnorm(N,
                                 mean = thetas,
                                 sd = sqrt(dat.bangertdrowns2004$vi),
                                 a = 1.96*sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_fixed_pb_logn


metafor::rma(yi, vi, data = dat)$I2
metafor::rma(yi, vi, data = dat)$H2

## =============================================================================
## Meta-cognition plotter
## =============================================================================

N = nrow(dat.bangertdrowns2004)
thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 1], N)
thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 2], N)
thetas0 = thetas0 + bangertdrowns2004$meta*thetas0_meta
sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_positive, N)
thetas = rnorm(N, thetas0, sigmas0)
probs = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unit, N)
hacked = rbinom(N, prob = probs, size = 1)
vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
  hacked*truncnorm::rtruncnorm(N,
                               mean = thetas,
                               sd = sqrt(dat.bangertdrowns2004$vi),
                               a = 1.96*sqrt(dat.bangertdrowns2004$vi))


plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = dat.bangertdrowns2004$meta + 1, pch = 20)
points(1/dat.bangertdrowns2004$vi, vals, col = dat.bangertdrowns2004$meta + 4, pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)

## Fixed, pb

N = nrow(dat.bangertdrowns2004)
thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 1], N)
thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 2], N)
thetas = thetas0 + bangertdrowns2004$meta*thetas0_meta
vals = (1 - hacked)*rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi)) +
  hacked*truncnorm::rtruncnorm(N,
                               mean = thetas,
                               sd = sqrt(dat.bangertdrowns2004$vi),
                               a = 1.96*sqrt(dat.bangertdrowns2004$vi))


plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = dat.bangertdrowns2004$meta + 1, pch = 20)
points(1/dat.bangertdrowns2004$vi, vals, col = dat.bangertdrowns2004$meta + 4, pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)

## Mixed, npb
N = nrow(dat.bangertdrowns2004)
thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 1], N)
thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_unbounded[, 2], N)
thetas0 = thetas0 + bangertdrowns2004$meta*thetas0_meta
sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_pb$stan_object)$beta_positive, N)
thetas = rnorm(N, thetas0, sigmas0)
vals = rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi))

plot(1/dat.bangertdrowns2004$vi, dat.bangertdrowns2004$yi, col = dat.bangertdrowns2004$meta + 1, pch = 20)
points(1/dat.bangertdrowns2004$vi, vals, col = dat.bangertdrowns2004$meta + 4, pch = 20)
lines(1/sort(dat.bangertdrowns2004$vi), 1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
lines(1/sort(dat.bangertdrowns2004$vi), -1.96*sqrt(sort(dat.bangertdrowns2004$vi)), lty = 2)
#plot(1/dat.bangertdrowns2004$vi, vals, col = dat.bangertdrowns2004$meta + 3, pch = 20)


H0.bridge <- bridgesampling::bridge_sampler(bangertdrowns2004_mixed_meta_pb$stan_object, silent = TRUE)
H1.bridge <- bridgesampling::bridge_sampler(bangertdrowns2004_mixed_meta_npb$stan_object, silent = TRUE)
bridgesampling::bf(H0.bridge, H1.bridge)

## =============================================================================
## Ppred
## =============================================================================

ppred = function(yi = NULL, data) {
  if(is.null(yi)) yi = data$z/sqrt(data$M)
  yi > data$lower/sqrt(data$M)
}


Q = function(yi = yi, vi = vi, data = NULL) {
  if(!is.null(data)) {
    yi = data[, deparse(substitute(yi))]
    vi = data[, deparse(substitute(vi))]
  }
  yi_w_bar = sum(1/vi*yi)/sum(1/vi)
  sum(1/vi*(yi - yi_w_bar)^2)
}

I2 = function(yi = yi, vi = vi, data = NULL) {
  if(!is.null(data)) {
    yi = data[, deparse(substitute(yi))]
    vi = data[, deparse(substitute(vi))]
  }
  Q_obs = Q(yi = yi, vi = vi, data = NULL)
  k = length(yi)
  max(0, (Q_obs - (k - 1))/Q_obs)
}


## =============================================================================
## Ppred KS test
## =============================================================================

kolmogorov = function(x, y) {
  length_x = length(x)
  length_y = length(y)
  x_and_y = c(x, y)
  z = cumsum(ifelse(order(x_and_y) <= length_x, 1/length_x, -1/length_y))
  max(abs(z[c(which(diff(sort(x_and_y)) != 0), length_x + length_y)]))
}

kolmogorov = function(x, y, test = TRUE) {
  length_x = length(x)
  length_y = length(y)
  x_and_y = c(x, y)
  z = cumsum(ifelse(order(x_and_y) <= length_x, 1/length_x, -1/length_y))
  statistic = max(abs(z[c(which(diff(sort(x_and_y)) != 0), length_x + length_y)]))
  if(test) 1 - .Call(stats:::C_pSmirnov2x, statistic, length_x, length_y)
  else statistic
}

kolmogorov(z1, z2, test = FALSE)