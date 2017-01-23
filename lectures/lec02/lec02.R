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




#### spurious correlation

yearly_divorce_rate_maine_per_1000 = c(5,4.7,4.6,4.4,4.3,4.1,4.2,4.2,4.2,4.1)
yearly_US_consumption_margarine_per_capita = c(8.2,7,6.5,5.3,5.2,4,4.6,4.5,4.2,3.7)
n = length(yearly_divorce_rate_maine_per_1000)
plot(1 : n, 
     yearly_divorce_rate_maine_per_1000, 
     type = "l", col = "red", ylab = "y", xlab = "time",
     ylim = c(min(yearly_divorce_rate_maine_per_1000, yearly_US_consumption_margarine_per_capita), max(yearly_divorce_rate_maine_per_1000, yearly_US_consumption_margarine_per_capita)))
points(1 : n, yearly_US_consumption_margarine_per_capita, type = "l", col = "blue")

x = yearly_divorce_rate_maine_per_1000
y = yearly_US_consumption_margarine_per_capita
plot(x, y)
#make a linear model
mod = lm(y ~ x)
summary(mod)
abline(mod)
# F-test says probability of seeing this linear relationship or 
# a relationship better correlated given no linear relationship 
# is about 1 in 100,000,000

#can we duplicate this? with enough time...

r = 0
min_pval = 1
while (TRUE){
  r = r + 1
  if (r %% 10000 == 0){cat("r =", r, "\n")}
  x = runif(n)
  #make a linear model
  mod = lm(y ~ x)
  #grab the pval of the t-test = F-test
  pval = coef(summary(mod))[2, 4]
  
  if (pval < min_pval){
    min_pval = pval
    plot(x, y, main = paste("pval =", signif(pval, 3), "r = ", r))
    abline(mod)
  }
  if (pval < 1e-8){
    break
  }
}






