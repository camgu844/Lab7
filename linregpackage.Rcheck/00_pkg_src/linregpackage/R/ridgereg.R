#' A function for ridge regression.
#'
#' @param formula A formula implies the model (e.g. y ~ x)
#' @param data data frame, dataset attached to the algorithm
#' @return ridgereg object.
#' @examples
#' formula <- Sepal.Length ~ Sepal.Width + Petal.Length
#' data <- iris
#' ret <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, iris)

ridgereg <- function(formula, data, lambda) {
  # stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))

  # Uses model.matrix and all.vars on data and formula and then normalizes all covariates
  X <- model.matrix(formula, data)
  #dependent variable y
  y_namn <- all.vars(formula, max.names=1L)
  y <- model.frame(formula, data)[[y_namn]] 

  #number of covariates
  n_cov <- length(colnames(X))

    #normalizes the covariates
  Xnorm <- matrix(nrow=length(y), ncol=n_cov)
  colnames(Xnorm) <- colnames(X)
  Xnorm[,1] <- 1
  for (i in 2:n_cov){
   Xnorm[,i] <- (X[,i]-mean(X[,i])) / sqrt(var(X[,i]))
  }

  #calculates regression coefficients:
  reg_coef <- solve(t(X)%*%X + diag(lambda,n_cov,n_cov)) %*% t(X) %*% y

  #calculates the fitted values:
  fitted_values <- X %*% reg_coef

  ret <- list()
  class(ret) <- "ridgereg"
  ret$reg_coef <- as.vector(reg_coef)
  ret$fitted_values <- fitted_values
  ret$formula <- formula
  ret$data <- data
  ret$data_name <- deparse(substitute(data))
  ret$lambda <- lambda

  return(ret)

}
