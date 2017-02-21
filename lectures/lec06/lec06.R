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
resample("regr.lm", white_wine_task, rdesc, measures = rmse)
#why doesn't lm overfit here??
resample("regr.randomForest", white_wine_task, rdesc, measures = rmse)
#why doesn't rf overfit (relative to its OOB estimate)?

#another example
X = read.csv("baseball.csv")



baseball_task = makeRegrTask(data = X, target = "salary_thou")
r = resample("regr.lm", baseball_task, rdesc, measures = rmse)
r = resample("regr.randomForest", baseball_task, rdesc, measures = rmse)

rf_mod = randomForest(salary_thou ~ ., X)
#oos R^2 and RMSE
rf_mod

#in-sample R^2
cor(predict(rf_mod, X), X$salary_thou)^2
#in-sample R^2 is not to be trusted on RF... RF overfits in-sample but does not overfit oos (weird... YES)

#another example
install.packages("mlbench")
library(mlbench)
#Wisconsin Breast Cancer Database
data(Sonar)
X = Sonar
colnames(X)


sonar_task = makeClassifTask(data = X, target = "Class")
r = resample("classif.multinom", sonar_task, rdesc, measures = acc)
r = resample("classif.randomForest", sonar_task, rdesc, measures = acc)

r = resample(makeLearner("classif.multinom", predict.type = "prob"), sonar_task, rdesc, measures = auc)
r = resample(makeLearner("classif.randomForest", predict.type = "prob"), sonar_task, rdesc, measures = auc)



