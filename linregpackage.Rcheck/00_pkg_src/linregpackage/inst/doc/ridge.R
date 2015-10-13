## ------------------------------------------------------------------------
library(linregpackage)
library(ggplot2)

data(iris)
attach(iris)
head(iris, 5)

## ------------------------------------------------------------------------
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
m = ridgereg(formula, iris, lambda=0.1)
summary(m)

## ----fig.width = 6, fig.height = 5, fig.align='center'-------------------
par(mfrow=c(2,1))
plot(predict(m),type='l',col='blue',main="Prediction")
plot(iris$Sepal.Length,type='l', col='red',main="Actual value")

