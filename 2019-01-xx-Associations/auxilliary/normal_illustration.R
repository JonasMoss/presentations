sigma = .5
alpha = 0
beta = .5

set.seed(313)
n = 100
x = sort(rnorm(n))
y = alpha + beta*x + rnorm(n, mean = 0, sd = sigma)

plot(x, y, pch = 20, ylim = c(-4, 4), xlim = c(-3, 3))
multiplier = 1


# 95 % probability lines, conditional
abline(a = 0, b = beta)
abline(a = multiplier*sigma, b = beta)
abline(a = -multiplier*sigma, b = beta)

# 95 % probability lines, unconditional
abline(h = multiplier*sqrt(sigma^2 + beta^2))
abline(h = -multiplier*sqrt(sigma^2 + beta^2))

sqrt(sigma^2 + beta^2)/sigma


plot(x, y, pch = 20, ylim = c(-4, 4), xlim = c(-3, 3))

# 95 % probability lines, conditional
abline(a = 0, b = beta)
abline(a = sigma^2, b = beta)
abline(a = -sigma^2, b = beta)

# 95 % probability lines, unconditional
abline(h = sigma^2 + beta^2)
abline(h = -(sigma^2 + beta^2))

(sigma^2 + beta^2)/sigma^2
