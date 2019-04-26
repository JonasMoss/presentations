library("tidyverse")

kvinner = scan("kvinner.csv")
menn = scan("menn.csv")
data = data.frame(leave = c(kvinner, menn), 
                  gender = c(rep("female", length(kvinner)),
                             rep("male", length(menn))),
                  x = rep(0:(length(menn) - 1)*pi/2, 2))


ml_means = c(menn = mean(menn), kvinner = mean(kvinner))
ml_sds = c(menn = sd(menn), kvinner = sd(kvinner))
p = 0.5


lm(leave ~ gender + gender*cos(x), data = data)


r2 = function(means, sds, pi) {
  value = 1 + pi*(1 - pi)*(means[1] - means[2])^2/(pi*sds[1]^2 + (1 - pi)*sds[2]^2)
  c(1 - 1/value)
}

r2(ml_means, ml_sds, p)
r2(ml_means, c(sqrt(mean(ml_sds^2)), sqrt(mean(ml_sds^2))), p)


plot(menn, ylim = c(min(menn) - 1, max(kvinner) + 1), col = "blue", type = "b",
     main = "Sickleave in Norway", pch = 20, ylab = "Percent sickleave", xlab = "Date")
points(kvinner, col = "red", type = "b", pch = 20)
x = 0:(length(menn) - 1)*pi/2
lines(predict(lm(menn ~ cos(x))))
lines(predict(lm(kvinner ~ cos(x))))
abline(h = ml_means[1], col = "blue", lty = 3)
abline(h = ml_means[2], col = "red", lty = 3)


f = function(t) {
  (alpha + beta*cos(t))^2/((alpha + beta*cos(t))^2 + psi) 
}


g = Vectorize(function(t, N = 30000) {
  x1 = rnorm(N, 7.675 + 1.11*cos(t), 0.4187)
  x2 = rnorm(N, 7.675 - 2.95 + (1.11 - 0.52)*cos(t), 0.4187)
  dat = data.frame(leave = c(x1, x2), 
                   gender = c(rep("female", length(x1)),
                              rep("male", length(x2))))
  summary(lm(leave ~ gender, data = dat))$r.sq
})

t = x
psi = 0.4187^2*4
beta = 0.52
alpha = 2.95

plot(t, f(t), type = "l")
