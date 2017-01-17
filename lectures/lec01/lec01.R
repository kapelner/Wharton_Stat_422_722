setwd("C:/Users/Adam/workspace/Wharton_Stat_422_722/lectures/lec01")
library(data.table)
#http://www4.stat.ncsu.edu/~boos/var.select/baseball.html
X = fread("baseball.csv")
X


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
mod
yhats = predict(mod, data.frame(x = x))
abline(mod)
for (i in 1 : n){
 segments(x[i], y[i], x[i], yhats[i], 
          lwd = 3,
          col = ifelse(y[i] - yhats[i] < 0, "blue", "red")) 
}


#variance before and after
set.seed(422)
n = 300
x = runif(n, 0, 3)
y = pmax(20000 + 20000 * x + rnorm(n, 0, 10000), 0)
hist(y, br = 50, xlab = "income", main = "")
abline(v = mean(y), col = "green", lwd = 5)
mean(y)
es = y - mean(y)
SSE_0 = sum(es^2)
SSE_0

plot(x, y, ylab = "income")
#now fit the linear model
mod = lm(y ~ x)
abline(mod)

##now find yhats
yhats = predict(mod, data.frame(x = x))
#and the es
for (i in 1 : n){
  segments(x[i], y[i], x[i], yhats[i], 
           lwd = 3,
           col = ifelse(y[i] - yhats[i] < 0, "blue", "red")) 
}
es = y - yhats
#now get SSE now
SSE_f = sum(es^2)
SSE_f

SSE_delta = SSE_0 - SSE_f
SSE_delta

#as a proportion of the original, null model
SSE_delta / SSE_0

summary(mod)$r.squared
