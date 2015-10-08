## ------------------------------------------------------------------------
library(linregpackage)
library(ggplot2)

data(iris)
attach(iris)
head(iris, 5)

## ------------------------------------------------------------------------
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
m = linreg(formula, iris)
summary(m)

## ----fig.width = 6, fig.height = 3, fig.align='center'-------------------
plot(m)

## ------------------------------------------------------------------------
summary(residuals(m))

## ------------------------------------------------------------------------
summary(predict(m))

