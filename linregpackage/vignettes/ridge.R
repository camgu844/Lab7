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

## ----fig.width = 6, fig.height = 3, fig.align='center'-------------------
plot(predict(m),col='blue')
plot(iris$Sepal.Length, col='red')

