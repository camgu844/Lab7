## ------------------------------------------------------------------------
library(linregpackage)
library(mlbench)
library(caret)

data(BostonHousing)
attach(BostonHousing)
head(BostonHousing, 5)

## ------------------------------------------------------------------------
# Rule of thumb: 60% for training, 40% for testing
index <- 1:nrow(BostonHousing)
set.seed(12082518)
inTraining <- createDataPartition(BostonHousing$medv, p = .60, list = FALSE)
trainset <- BostonHousing[ inTraining,]
testset  <- BostonHousing[-inTraining,]
print(nrow(trainset))
print(nrow(testset))

## ------------------------------------------------------------------------
lmfit <- train(medv ~ ., data = trainset, 
               method = "lm")
print(lmfit)

## ------------------------------------------------------------------------
lmforward = train(medv ~ ., data = trainset,
                 method = "leapForward")
print(lmforward)

## ----fig.width = 6, fig.height = 5, fig.align='center'-------------------
modelinfo = list(
  library=c('linregpackage'),
  type=c("Regression"),
  parameters=data.frame(parameter = c("lambda"),
                        class = c("numeric"),
                        label = c("Lambda")),
  fit=function(x, y, wts, param, lev, last, weights, classProbs, ...){
    print(y)
    return (ridgereg(y~x, data=NULL, param))
  },
  predict=function(modelFit, newdata, preProc = NULL, submodels = NULL){
    return (predict(modelFit, newdata))
  },
  grid=function(x, y, len = NULL, search = "grid"){
    return (data.frame(lambda=1:10/10))
  },
  prob=NULL
)
# lmridge = train(medv ~ ., data = trainset, method = modelinfo)
# m = ridgereg(formula, iris, lambda=0.1)
# par(mfrow=c(2,1))
# plot(predict(m),type='l',col='blue',main="Prediction")
# plot(iris$Sepal.Length,type='l', col='red',main="Actual value")

## ------------------------------------------------------------------------
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

