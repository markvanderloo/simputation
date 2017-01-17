
#' Impute by variable derivation
#' 
#' Impute missing values by copying from another variable or a transformations
#' of other variables.
#' 
#' @param dat \code{[data.frame]}, with variables to be imputed and their
#'   predictors.
#' @param formula \code{[formula]} imputation model description (See Model description) 
#' @param add_residual \code{[character]} Type of residual to add. \code{"normal"} 
#'   means that the imputed value is drawn from \code{N(mu,sd)} where \code{mu}
#'   and \code{sd} are estimated from the model's residuals (\code{mu} should equal
#'   zero in most cases). If \code{add_residual = "observed"}, residuals are drawn
#'   (with replacement) from the model's residuals. Ignored for non-numeric 
#'   predicted variables.
#' @param ... Currently unused
#' 
#' @export
#' 
#' @examples
#' irisNA <- iris
#' irisNA[1:3,1] <- irisNA[3:7,2] <- NA
#' 
#' # copy a value from another variable (where available)
#' a <- impute_proxy(irisNA, Sepal.Width ~ Sepal.Length)
#' head(a)
#' 
#' # group mean imputation
#' a <- impute_proxy(irisNA
#'   , Sepal.Length ~ mean(Sepal.Length,na.rm=TRUE) | Species)
#' head(a)
#' 
#' # ratio imputation (but use impute_lm for that)
#' a <- impute_proxy(irisNA, 
#'   Sepal.Length ~ mean(Sepal.Length,na.rm=TRUE)/mean(Sepal.Width,na.rm=TRUE) * Sepal.Width)
#' 
impute_proxy <- function(dat, formula, add_residual = c("none","observed","normal"), ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  groups <- groups(formula, dat = dat)
  predicted <- get_imputed(formula,names(dat))
  
  proxy <- function(d){
    v <- eval(formula[[3]],envir=d)
    if (length(v)==1) v <- rep(v,nrow(d))
    v
  } 
  
  imp_val <- if (has_groups(formula)){
    formula <- remove_groups(formula)
    unsplit(lapply(split(dat, dat[groups]), proxy), dat[groups])
  } else {
    proxy(dat)
  }
  if(length(imp_val) != nrow(dat) ){
    warnf("Right-hand-side of\n %s\n must evaluate to vector of length %d or 1. Returning original data"
          , deparse(formula), nrow(dat))
    return(dat)
  }
  for ( p in predicted){
    ina <- is.na(dat[,p])
    dat[ina,p] <- imp_val[ina] + 
      get_res(sum(ina), imp_val[!ina] - dat[!ina,p], type=add_residual)
  }
  dat
}

