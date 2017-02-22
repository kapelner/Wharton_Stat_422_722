x = c(2.47,.57,.84,2.18)
y = c(.5,1.95, 1.91, 2.51)
X = data.frame(x, y)

mod = lm(y ~ x)
summary(mod)