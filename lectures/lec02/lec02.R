#### epsilon = ignorance

n = 100
p = 1000
sigma = 0.1
x = rnorm(n)
U = matrix(runif(n * p, -0.2, 0.2), nrow = n)

#no noise case
f_of_x = 2 + 3 * x
t_of_xs = f_of_x
y = t_of_xs
plot(x, y)

#a little noise case
eps = rowSums(U[, 1:1000, drop = FALSE])
t_of_xs = f_of_x + eps
y = t_of_xs
plot(x, y)
summary(lm(y ~ x))


##### LRT vs F-test
set.seed(422)
n = 100
p = 10
sigma = 2
X = matrix(runif(n * p), nrow = n)
beta = rep(1, p)
y = X %*% as.matrix(beta) + rnorm(n, 0, sigma)
mod_full = lm(y ~ X)
summary(mod_full)
mod_red = lm(y ~ 1)
summary(mod_red)
anova(mod_red, mod_full, test = "LRT")
anova(mod_red, mod_full, test = "F")


###rainfall (z), umbrellas (X) and car accidents (y) example
set.seed(422)
n = 100
sigma = 0.3
z = runif(n, 0, 6)
x = z^2 + rnorm(n, sigma)
y = z + rnorm(n, sigma)
plot(x, y, xlab = "umbrellas", ylab = "accidents (100s)")
mod = lm(y ~ x)
abline(mod)
summary(mod)

mod = lm(y ~ x + z)
summary(mod)

#accidents vs umbrellas with rainfall constant
z_low = z < quantile(z, 0.1)

mod = lm(y[z_low] ~ x[z_low])
plot(x[z_low], y[z_low], xlab = "umbrellas", ylab = "accidents (100s)")
abline(mod)
summary(mod)

z_low = quantile(z, 0.1) < z & z < quantile(z, 0.2)

mod = lm(y[z_low] ~ x[z_low])
plot(x[z_low], y[z_low], xlab = "umbrellas", ylab = "accidents (100s)")
abline(mod)
summary(mod)

