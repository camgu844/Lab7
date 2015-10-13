library(linregpackage)
library(MASS)
library(dplyr)

context("tests the function ridgereg")

data(iris)
formula <- Sepal.Length ~  Sepal.Width + Petal.Length + Petal.Width
X = dplyr::select(iris, Sepal.Width, Petal.Length, Petal.Width)
y = iris$Sepal.Length
form1 = y~as.matrix(X)
data <- iris

m1 = ridgereg(formula, data, 1)
m2 = lm.ridge(formula, data, lambda=1)
m3 = ridgereg(form1,NULL,1)

test_that("Class returned by ridgereg", {
  expect_that(class(m1), equals("ridgereg"))
})

context("Tests the methods coefficients, resid and pred")

test_that("Coefficients", {
  expect_less_than(sum(as.numeric(abs(coef(m1))-abs(coef(m2)))),0.01)
})

test_that("Predictions", {
  expect_less_than(sum(as.numeric(abs(predict(m1))-abs(predict(m3)))),0.01)
})
