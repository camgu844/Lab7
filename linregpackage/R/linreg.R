#' Multiple Linear Regression.
#'
#' @param formula A formula implies the model (e.g. y ~ x)
#' @param data data frame, dataset attached to the algorithm
#' @return linreg object.
#' @examples
#' formula <- eruptions ~ waiting
#' data <- faithful
#' m1 = linreg(eruptions~waiting, faithful)
#'
#' summary(m1)
#' # Call:
#' # linreg( eruptions ~ waiting )
#' #
#' # Coefficients:
#' #                Estimate  Std. Error   t value Pr(>|t|)
#' # (Intercept) -1.87401599 0.160143302 -11.70212        0
#' # waiting      0.07562795 0.002218541  34.08904        0
#' #
#' # Residual standard error: 0.4965 on 270 degrees of freedom
linreg <- function (formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))


  # uses model.matrix and all.vars on data and formula, and then calculates the regressions coefficients.

  X <- model.matrix(formula, data)

  y_namn <- all.vars(formula, max.names=1L)

  y <- data[[y_namn]]

  del_a <- t(X) %*% X
  del_b <- solve(del_a)  # solve(A) 	Inverse of A where A is a square matrix.

  reg_coef <- del_b %*% t(X) %*% y  # "Regressions coefficients"

  fitted_values <- X %*% reg_coef  # "The fitted values"

  resi <- as.vector(y - fitted_values) # "The residuals"

  n <- length(y)
  p <- length(colnames(X))
  deg_free <- n - p         # "The degrees of freedom"

  res_var <- as.vector((t(resi) %*% resi) / deg_free)  # "The residual variance"

  var_reg_coef <- res_var * del_b  # "The variance of the regression coefficients"

  t_each_coef <- reg_coef / sqrt(diag(var_reg_coef))    # "The t-values for each coefficient"

  p_values <- 2*pt(-abs(t_each_coef), df = deg_free)   # "p-values for the regressions coefficients"
  
  
  ret <- list()
  class(ret) <- "linreg"
  ret$formula <- formula
  ret$data <- data
  ret$data_name <- deparse(substitute(data))
  ret$reg_coef <- reg_coef[,1]
  ret$fitted_values <- as.vector(fitted_values)
  ret$resi <- resi
  ret$deg_free <- deg_free
  ret$res_var <- res_var
  ret$var_reg_coef <- sqrt(diag(var_reg_coef))
  ret$t_each_coef <- t_each_coef
  ret$p_values <- p_values
  return(ret)
}