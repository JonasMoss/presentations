sigma = 1
alpha = 0
beta = 2

set.seed(313)
n = 100
x = sort(rnorm(n))
y = alpha + beta*x + rnorm(n, mean = 0, sd = sigma)

plot(x, y, pch = 20, ylim = c(-4, 4), xlim = c(-3, 3), bty = "l")
multiplier = 1


# 95 % probability lines, conditional
abline(a = 0, b = beta)
abline(a = multiplier*sigma, b = beta)
abline(a = -multiplier*sigma, b = beta)
# 
# # 95 % probability lines, conditional
# abline(a = 0, b = beta)
# abline(h = multiplier*sigma)
# abline(h = -multiplier*sigma)

# Added lines
abline(h = 2*multiplier*sigma - multiplier*sqrt(sigma^2 + beta^2), col = "grey")

# 95 % probability lines, unconditional
abline(h = multiplier*sqrt(sigma^2 + beta^2))
abline(h = -multiplier*sqrt(sigma^2 + beta^2))

abline(v = multiplier*(sigma - sqrt(sigma^2 + beta^2))/beta)

gnosis = sqrt(sigma^2 + beta^2)/sigma
rsq = 1 - 1/gnosis^2
corr = sqrt(rsq)


# 
# plot(x, y, pch = 20, ylim = c(-4, 4), xlim = c(-3, 3))
# 
# # 95 % probability lines, conditional
# abline(a = 0, b = beta)
# abline(a = sigma^2, b = beta)
# abline(a = -sigma^2, b = beta)
# 
# # 95 % probability lines, unconditional
# abline(h = sigma^2 + beta^2)
# abline(h = -(sigma^2 + beta^2))
# 
# (sigma^2 + beta^2)/sigma^2
