library(data.table)
X = fread("medicorp.csv")
head(X)
mod_2 = lm(SALES ~ poly(BONUS, 2), X)
mod_4 = lm(SALES ~ poly(BONUS, 4), X)
anova(mod_2, mod_4, test = "LRT")


######## From previously...
set.seed(422)
n = 100

sigma = 5
x = seq(0, 10, length.out = n)
y = x + rnorm(n, 0, sigma)
plot(x, y)
mod = lm(y ~ x)
abline(mod)
summary(mod)

set.seed(421)
p = 100
Xjunk = matrix(rnorm(n*p), nrow = n)
X = cbind(x, Xjunk)
colnames(X) = c("real", rep("fake", p))

mod = lm(y ~ X[, 1:98])
abline(mod)
summary(mod)

##ditto with just polynomials
library(splines)
mod = lm(y ~ bs(x, df = 98))
plot(x, y)
xstars = seq(min(x), max(x), length.out = 1000)
yhats = predict(mod, data.frame(x = xstars))
points(xstars, yhats, type = "l")
summary(mod)

###overfitting demo
n = 2
set.seed(422)

sigma = 0.5
x = c(1, 2)
f_of_x = x
y = f_of_x + rnorm(n, 0, sigma)
plot(x, y)
mod = lm(y ~ x)
abline(mod, col = "brown")
for (i in 1 : n){
  segments(x[i], y[i], x[i], f_of_x[i], col = "red")
}
points(x, f_of_x, col = "green", type = "l")
summary(mod)


#### overfitting again
##show overfit model
set.seed(422)
n = 100
sigma = 5
x = seq(0, 10, length.out = n)
f_of_x = x
y = f_of_x + rnorm(n, 0, sigma)
library(splines)
mod = lm(y ~ bs(x, df = 98))
summary(mod)
plot(x, y, type = "n")
xstars = seq(min(x), max(x), length.out = 1000)
yhats = predict(mod, data.frame(x = xstars))
points(xstars, yhats, type = "l", col = "grey")
#make new data
nstar = 20
xstar = seq(0, 10, length.out = nstar)
ystar = xstar + rnorm(nstar, 0, sigma)
points(xstar, ystar, lwd = 3)
#plot overfit predictions
ystar_hat_over = predict(mod, data.frame(x = xstar))
for (i in 1 : nstar){
  segments(xstar[i], ystar[i], xstar[i], ystar_hat_over[i], col = "red")
}
#plot non-overfit predictions - overfittedness is in red
simple_mod = lm(y ~ x)
abline(simple_mod, col = "blue")
ystar_hat = predict(simple_mod, data.frame(x = xstar))
for (i in 1 : nstar){
  segments(xstar[i] + 0.01, ystar[i], xstar[i] + 0.01, ystar_hat[i], col = "purple", lwd = 2)
}
#calculate oosSSEs
sse_overfit = sum((ystar - ystar_hat_over)^2)
sse_not_overfit = sum((ystar - ystar_hat)^2)
sse_overfit
sse_not_overfit
sse_overfit / sse_not_overfit
#calculate oosRMSEs
sqrt(sse_overfit / nstar)
sqrt(sse_not_overfit / nstar)
#calculate oosRsq's
Rsq_overfit = cor(ystar_hat_over, ystar)^2
Rsq_not_overfit = cor(ystar_hat, ystar)^2
Rsq_overfit
Rsq_not_overfit