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

## ------------------------------------------------------------------------
modelinfo = list(
  library=c('linregpackage'),
  type=c("Regression"),
  parameters=data.frame(parameter = c("lambda"),
                        class = c("numeric"),
                        label = c("Lambda")),
  fit=function(x, y, wts, param, lev, last, weights, classProbs, ...){
    fitted= (ridgereg(y~x, data=NULL, param))
    return(fitted)
  },
  predict=function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predictions = predict(modelFit, newdata)
    return(predictions)
  },
  grid=function(x, y, len = NULL, search = "grid"){
    return (data.frame(lambda=seq(0,1000,50)/1000))
  },
  prob=NULL
)
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)
lmridge = train(medv ~ ., data = trainset, 
                method = modelinfo,
                trControl=fitControl)
print(lmridge)


## ----fig.width = 6, fig.height = 7, fig.align='center'-------------------
pfit = predict(lmfit, testset)
pforward = predict(lmforward, testset)
pridge = predict(lmridge, testset)
actual = testset$medv
par(mfrow=c(4,1))
plot(pfit,type='l',col='blue',main="LMFIT Prediction")
plot(pforward,type='l',col='blue',main="LMFORWARD Prediction")
plot(pridge,type='l',col='blue',main="LMRIDGE Prediction")
plot(actual, type='l', col='red',main="Actual value")

## ------------------------------------------------------------------------
print(paste('RMSE lmfit:', sqrt(sum((actual-pfit)^2)/length(actual)) ))
print(paste('RMSE lmforward:', sqrt(sum((actual-pforward)^2)/length(actual)) ))
print(paste('RMSE ridge:', sqrt(sum((actual-pridge)^2)/length(actual)) ))

