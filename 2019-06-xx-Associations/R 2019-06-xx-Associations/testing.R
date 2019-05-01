
covariates = model.matrix(mpg ~ cyl + disp, data = mtcars)
theta = coef(lm(mpg ~ cyl + disp, data = mtcars))

covariates = scale(covariates, center = TRUE, scale = FALSE)

X = covariates%*%theta
b = c(crossprod(X, X))/nrow(X)
sigma = summary(lm(mpg ~ cyl + disp, data = mtcars))$sigma
1 - sigma^2/(b + sigma^2)
