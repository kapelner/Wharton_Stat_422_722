
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