# install.packages("mlr")
library(mlr) #KILLER NEW PACKAGE

K_FOLDS = 10

#load the data
X = read.csv("white_wine.csv")
dim(X)
#run a simple regression (model A)
mod = lm(quality ~ ., X)
summary(mod)

#create
white_wine_learning_task = makeRegrTask(data = X, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
r
r$pred$data[1 : 500, ]
nrow(X)
sqrt(r$aggr)
#thus we are not overfitting


#now let's do the other models (B)
#first save the polys
poly_degree = 3
poly_formula = as.formula( ~ poly(fixed.acidity, poly_degree) + 
                         poly(volatile.acidity, poly_degree) + 
                         poly(citric.acid, poly_degree) + 
                         poly(residual.sugar, poly_degree) + 
                         poly(chlorides, poly_degree) +
                         poly(free.sulfur.dioxide, poly_degree) + 
                         poly(total.sulfur.dioxide, poly_degree) +
                         poly(density, poly_degree) + 
                         poly(pH, poly_degree) + 
                         poly(sulphates, poly_degree) + 
                         poly(alcohol, poly_degree))
mod_matrix = data.frame(model.matrix(poly_formula, X))
mod_matrix[, 1] = X$quality
names(mod_matrix)[1] = "quality"

white_wine_learning_task = makeRegrTask(data = mod_matrix, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
sqrt(r$aggr)

install.packages("lasso2")
library(lasso2)


##C
form = merge.formula(as.formula(quality ~ .^2), poly_formula)
mod_matrix = data.frame(model.matrix(form, X))
mod_matrix[, 1] = X$quality
colnames(mod_matrix)[1] = "quality"
# head(mod_matrix)

white_wine_learning_task = makeRegrTask(data = mod_matrix, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
sqrt(r$aggr)

##D
form = merge.formula(as.formula(quality ~ .^3), poly_formula)
mod_matrix = data.frame(model.matrix(form, X))
mod_matrix[, 1] = X$quality
colnames(mod_matrix)[1] = "quality"
# head(mod_matrix)

white_wine_learning_task = makeRegrTask(data = mod_matrix, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
sqrt(r$aggr)

##E
form = merge.formula(as.formula(quality ~ .^4), poly_formula)
mod_matrix = data.frame(model.matrix(form, X))
mod_matrix[, 1] = X$quality
colnames(mod_matrix)[1] = "quality"
# head(mod_matrix)

white_wine_learning_task = makeRegrTask(data = mod_matrix, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
sqrt(r$aggr)

##F
form = merge.formula(as.formula(quality ~ .^11), poly_formula)
mod_matrix = data.frame(model.matrix(form, X))
mod_matrix[, 1] = X$quality
colnames(mod_matrix)[1] = "quality"
# head(mod_matrix)

white_wine_learning_task = makeRegrTask(data = mod_matrix, target = "quality")
#make a 10-fold
rdesc = makeResampleDesc("CV", iters = K_FOLDS)
r = resample("regr.lm", white_wine_learning_task, rdesc)
sqrt(r$aggr)






