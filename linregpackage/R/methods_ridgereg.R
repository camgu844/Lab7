#' A method for printing objects of class ridgereg.
#' 
#' @param x ridgereg object
print.ridgereg <- function(x) {
  data_name <- x$data_name
  lambda <- x$lambda
  cat("Call:\n")
  cat(paste("ridgereg(formula = ", deparse(x$formula), ", data = ", data_name, ", ", lambda,")\n")) 
  cat("\nCoefficients:\n")
  print(x$reg_coef)
}

#' A method that returns the coefficients.
#'
#' @param x ridgereg object
#' @return the regression coefficients.
coef.ridgereg <- function(x){
  return(x$reg_coef)
}

#' A method that returns the predicted values y.
#'
#' @param x ridgereg object
#' @return vector of predictions.
predict.ridgereg <- function(x, newdata=NULL){
  if (!is.null(newdata))
    return(as.vector(rowSums(x$reg_coef * newdata)))
  return(as.vector(x$fitted_values))
}