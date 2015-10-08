#' A function for ridge regression.
#' 
#' @param formula A formula implies the model (e.g. y ~ x)
#' @param data data frame, dataset attached to the algorithm
#' @return ridgereg object.
#' @examples 

ridgereg <- function(formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))
 
  # Uses model.matrix and all.vars on data and formula and then normalizes all covariates
  
  X <- model.matrix(formula, data)
  #number of covariates
  n_cov <- length(colnames(X))
  #dependent variable y
  y_namn <- all.vars(formula, max.names=1L)
  y <- data[[y_namn]] 
  #normalizes the covariates
  Xnorm <- matrix(nrow=length(y), ncol=n_cov)
  colnames(Xnorm) <- colnames(X)
  Xnorm[,1] <- 1
  for (i in 2:n_cov){
   Xnorm[,i] <- (X[,i]-mean(X[,i])) / sqrt(var(X[,i]))
  }
  
  #calculates regression coefficients:
  del_a <- (t(X) %*% X) + (lambda %*% I)  # what is lambda and I?????????
  reg_coef <- solve(del_a) %*% t(X) %*% y
  
  #calculates the fitted values:
  fitted_values <- X %*% reg_coef
  
  ret <- list()
  class(ret) <- "ridgereg"
  ret$reg_coef <- reg_coef
  ret$fitted_values <- fitted_values
  
  return(ret)

}