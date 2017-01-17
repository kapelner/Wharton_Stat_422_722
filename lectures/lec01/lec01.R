setwd("C:/Users/Adam/workspace/Wharton_Stat_422_722/lectures/lec01")
library(data.table)
#http://www4.stat.ncsu.edu/~boos/var.select/baseball.html
X = fread("baseball.csv")
X

# mod = lm(salary_thou ~ ., X)
# summary(mod)
# 
# mod = lm(log(salary_thou) ~ ., X)
# summary(mod)

n = 100
sigma = 2
set.seed(422.722)

x = seq(0, 10, length.out = n)
f_of_x = 3 + 1.5 * x #true... pretend it's hidden
e = rnorm(n, 0, sigma)
y = f_of_x + e
Xy = data.frame(x, y) #make dataframe

#parametric view: assume simple model: the straight line
mod = lm(y ~ ., Xy)
plot(x, y)
abline(mod)

#nonparametric view: allow model to be complex

mod = lm(y ~ poly(x, 20), Xy)
plot(x, y)
xstars = seq(min(x), max(x), length.out = 10000)
yhats = predict(mod, data.frame(x = xstars))
points(xstars, yhats, type = "l")



#example fits
n = 10
sigma = 2
set.seed(401)

x = seq(0, 1, length.out = 10)
y = 5 + 2 * x + rnorm(n, 0, sigma)
plot(x, y, ylim = c(0, 12))
mod = lm(y ~ x)
yhats = predict(mod, data.frame(x = x))
abline(mod)
for (i in 1 : 10){
 segments(x[i], y[i], x[i], yhats[i], 
          lwd = 3,
          col = ifelse(y[i] - yhats[i] < 0, "blue", "red")) 
}

