X = read.csv("white_wine.csv")

lin_mod = lm(quality ~ ., X)
summary(lin_mod)

# install.packages("randomForest")
library(randomForest)
rf_mod = randomForest(quality ~ ., X)
rf_mod

#overfitting?
library(mlr)
K_FOLDS = 5 #just to save time
rdesc = makeResampleDesc("CV", iters = K_FOLDS)

white_wine_task = makeRegrTask(data = X, target = "quality")
r = resample("regr.lm", white_wine_task, rdesc, measures = rmse)
r = resample("regr.randomForest", white_wine_task, rdesc, measures = rmse)


#another example
X = read.csv("baseball.csv")

lin_mod = lm(salary_thou ~ ., X)
summary(lin_mod)

rf_mod = randomForest(salary_thou ~ ., X)
rf_mod

baseball_task = makeRegrTask(data = X, target = "salary_thou")
r = resample("regr.lm", baseball_task, rdesc, measures = rmse)
r = resample("regr.randomForest", baseball_task, rdesc, measures = rmse)
