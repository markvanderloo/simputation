
#' Linear regression imutation
#'
#' Use to fit and impute missing data.
#'
#' @param data The data
#' @param x a \code{\link[stats]{formula}} object.
#' @param ... further arguments passed to \code{\link[stats]{lm}} or \code{\link{rlm}}
#' 
#' @section Details:
#' 
#' Model specification works as usual, except that it is possible to impute multiple
#' variables based on the same model. To specify the same model for multiple variables,
#' simply add variables to the left-hand side of the formula using \code{+}. Also see the
#' examples.
#' 
#' If a value cannot be imputed because one of its predictors is missing, the value will
#' remain missing after imputation.
#' 
#' 
#' @references 
#' 
#' Linear models are fit with the \code{stats::lm} function. Robust linear models
#' are fit with \code{MASS::rlm}.
#' 
#' @examples
#' 
#' data(iris)
# irisNA <- iris
# irisNA[1:4, "Sepal.Length"] <- NA
# irisNA[3:7, "Sepal.Width"] <- NA
# 
# # impute a single variable (Sepal.Length)
# i1 <- impute_lm(irisNA, Sepal.Length ~ Sepal.Width + Species)
# 
# # impute both Sepal.Length and Sepal.Width, using robust imputation
# i2 <- impute_lm(irisNA, Sepal.Length + Sepal.Width ~ Species + Petal.Length)
#' 
#' 
#' @export
impute_lm <- function(data, x, ...){
  stopifnot(inherits(x,"formula"))
  predicted <- all.vars(x[[2]])
  formulas <- paste(predicted, "~" ,deparse(x[[3]]) )  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(data[p])
    if (!any(ina)) next # skip if no missings
    m <- stats::lm(as.formula(formulas[i]), data=data, ...)
    data[ina, p] <- stats::predict(m, newdata=data[ina,,drop=FALSE])
  }
  data
}


#' @rdname impute_lm
#' @export
impute_rlm <- function(data, x, ...){
  stopifnot(inherits(x,"formula"))
  predicted <- all.vars(x[[2]])
  formulas <- paste(predicted, "~" ,deparse(x[[3]]) )  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(data[p])
    if (!any(ina)) next # skip if no missings
    m <- MASS::rlm(as.formula(formulas[i]), data=data, ...)
    data[ina, p] <- stats::predict(m, newdata=data[ina,,drop=FALSE])
  }
  data
}

# TODO:
# impute_median
# impute_const
# ...
