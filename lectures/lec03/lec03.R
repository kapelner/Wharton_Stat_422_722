


############ extrapolation
n = 25
sigma = 1
x = runif(n, 10, 15)
y = x + rnorm(n, 0, sigma)


plot(x, y, xlim = c(0, 25), ylim = c(0, 25))

lin_mod = lm(y ~ x)
abline(lin_mod, col = "blue")

poly_mod = lm(y ~ poly(x, 6))
xstars = seq(0, 25, length.out = 10000)
yhats = predict(poly_mod, data.frame(x = xstars))
points(xstars, yhats, type = "l", col = "green")

install.packages("rpart")
library(rpart)
dtree_mod = rpart(y ~ x)
xstars = seq(0, 25, length.out = 10000)
yhats = predict(dtree_mod, data.frame(x = xstars))
points(xstars, yhats, type = "l", col = "brown")


##########opt design for slope
plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")

beta_0 = 1.5
beta_1 = 2
sigma = 0.6
n = 10
Nsim = 500

for (r in 1 : Nsim){
  x = runif(n)
  #x = seq(0, 1, length.out = n)
  #x = c(rep(0, 5), rep(1, 5))
  y = beta_0 + beta_1 * x + rnorm(n, 0, sigma)
  mod = lm(y ~ x)
  abline(mod, col = "grey")
}
abline(a = beta_0, b = beta_1, col = "green")

##recall polynomial regression
n = 100
sigma = 2
set.seed(422.722)

x = seq(0, 10, length.out = n)
f_of_x = 3 + 1.5 * x #true... pretend it's hidden
e = rnorm(n, 0, sigma)
y = f_of_x + e
Xy = data.frame(x, y)
mod = lm(y ~ poly(x, 20), Xy)
plot(x, y)
xstars = seq(min(x), max(x), length.out = 10000)
yhats = predict(mod, data.frame(x = xstars))
points(xstars, yhats, type = "l")












