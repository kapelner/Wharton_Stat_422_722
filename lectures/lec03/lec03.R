
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















library(data.table)
#http://www4.stat.ncsu.edu/~boos/var.select/baseball.html
X = fread("baseball.csv")
X


train_idx = unique(sample(1 : n, replace = TRUE))
test_idx = setdiff(1 : n, train_idx)
ytest = X[test_idx, "salary_thou"]

lm_mod = lm(log(salary_thou) ~ ., X[train_idx, ])
summary(lm_mod)$r.squared

library(randomForest)
rf_mod = randomForest(log(salary_thou) ~ ., X)

y_hat = predict(lm_mod, X[test_idx, ])
cor(y_hat, log(ytest))^2


y_hat = predict(rf_mod, X[test_idx, ])
cor(y_hat, log(ytest))^2
rf_mod