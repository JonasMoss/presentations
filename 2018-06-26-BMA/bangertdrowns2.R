### ============================================================================
### Bangert-Drowns files.
### ============================================================================
replicate(1000, {
  N = nrow(dat.bangertdrowns2004)
  thetas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_npb$stan_object)$beta_unbounded[, 1], N)
  thetas0_meta = sample(rstan::extract(bangertdrowns2004_mixed_meta_npb$stan_object)$beta_unbounded[, 2], N)
  thetas0 = thetas0 + bangertdrowns2004$meta*thetas0_meta
  sigmas0 = sample(rstan::extract(bangertdrowns2004_mixed_meta_npb$stan_object)$beta_positive, N)
  thetas = rnorm(N, thetas0, sigmas0)
  vals = rnorm(N, mean = thetas, sd = sqrt(dat.bangertdrowns2004$vi))
  c(I2(vals, dat$vi), 
    Q(vals, dat$vi), 
    mean(ppred(vals, data = bangertdrowns2004)),
    kolmogorov(vals, dat.bangertdrowns2004$yi))
}) -> res_mixed_meta_npb

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
}) -> res_mixed_meta_pb


hist(res_fixed_pb_logn[4 ,])
hist(res_mixed_meta_pb[4 ,])
hist(res_mixed_meta_npb[4 ,])

hist(res_mixed_pb_logn[3 ,], col = rgb(1, 0, 0, 0.1), freq = FALSE)
hist(res_mixed_meta_pb[3 ,], col = rgb(0, 0, 1, 0.1), freq = FALSE, add = TRUE)
hist(res_mixed_meta_npb[3 ,], col = rgb(0, 1, 0, 0.1), freq = FALSE, add = TRUE)
