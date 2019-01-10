
#' Impute by variable derivation
#' 
#' Impute missing values by a constant, by copying another variable computing 
#' transformations from other variables.
#' 
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
#' @section Model Specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. 
#' 
#' For \code{impute_const}, the \code{MODEL_SPECIFICATION} is a single
#' value and \code{GROUPING_VARIABLES} are ignored.
#' 
#' For \code{impute_proxy}, the \code{MODEL_SPECIFICATION} is a variable or
#' expression in terms of variables in the dataset that must result in either a
#' single number of in a vector of length \code{nrow(dat)}.
#' 
#' If grouping variables are specified, the data set is split according to the 
#' values of those variables, and model estimation and imputation occur 
#' independently for each group.
#' 
#' Grouping using \code{dplyr::group_by} is also supported. If groups are 
#' defined in both the formula and using \code{dplyr::group_by}, the data is 
#' grouped by the union of grouping variables. Any missing value in one of the 
#' grouping variables results in an error.
#' 
#'    
#' @export
#' 
#' @examples
#' irisNA <- iris
#' irisNA[1:3,1] <- irisNA[3:7,2] <- NA
#'
#' # impute a constant 
#' 
#' a <- impute_const(irisNA, Sepal.Width ~ 7)
#' head(a)
#' 
#' a <- impute_proxy(irisNA, Sepal.Width ~ 7)
#' head(a)
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
#' # random hot deck imputation
#' a <- impute_proxy(irisNA, Sepal.Length ~ mean(Sepal.Length, na.rm=TRUE)
#' , add_residual = "observed")
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


#' @rdname impute_proxy
#' @export
impute_const <- function(dat, formula, add_residual = c("none","observed","normal"),...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  formula <- remove_groups(formula)
  if (length(formula[[3]]) != 1)
    stop(sprintf("Expected constant, got '%s'",deparse(formula[[3]])))
  const <- as.numeric(deparse(formula[[3]]))
  
  if (is.na(const)) const <- deparse(formula[[3]])
  predicted <- get_imputed(formula, dat)
  for ( p in predicted ){
    ina <- is.na(dat[p])
    nmiss <- sum(ina)
    # prevent conversion of constant to NA from popping up: we just replace NA 
    # with NA in that case.
    tryCatch(
      dat[ina,p] <- if ( add_residual == "none" ){
        const
      } else if (nmiss == 0){
        warning("All values missing, so no random residuals added")
        const
      } else {
        const + get_res(nmiss=nmiss, residuals=const-dat[!ina,p], type=add_residual)
      }
      , warning=function(w){}
    )
  }
  dat
}




#' Impute (group-wise) medians
#' 
#' Impute medians of group-wise medians.
#' 
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
#' @param type \code{[integer]} Specifies the algorithm to compute the median.
#'   See the 'details' section of \code{\link[stats]{quantile}}.
#' @param ... Currently not used.
#'
#' @section Model Specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. Variables in \code{MODEL_SPECIFICATION} and/or 
#' \code{GROUPING_VARIABLES} are used to split the data set into groups prior to
#' imputation. Use \code{~ 1} to specify that no grouping is to be applied.
#' 
#' @examples
#'
#' # group-wise median imputation
#' irisNA <- iris
#' irisNA[1:3,1] <- irisNA[4:7,2] <- NA
#' a <- impute_median(irisNA, Sepal.Length ~ Species)
#' head(a)
#'
#' # group-wise median imputation, all variables except species
#' 
#' a <- impute_median(irisNA, . - Species ~ Species)
#' head(a)
#'
#' @export
impute_median <- function(dat, formula
        , add_residual = c("none","observed","normal")
        , type=7, ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)

  predicted <- get_imputed(formula, dat)
  predicted <- predicted[sapply(dat[predicted], is.numeric)]
  predictors <- groups(dat,formula)
  formula <- remove_groups(formula)
  predictors <- unique( c(predictors, get_predictors(formula, dat, one_ok=TRUE)) )
  
  # compute formula values
  by <- if (length(predictors) == 0) list(rep(1,nrow(dat))) else as.list(dat[predictors])
  # silence the warning about producing NA's (give specific warning later)
  medians <- withCallingHandlers(
    stats::aggregate(dat[predicted],by=by, FUN=stats::quantile, p=0.5, na.rm=TRUE)
    , warning=function(w) invokeRestart("muffleWarning") 
  )
  # create nrow(data) X npredictors data.frame with formula values.
  if (length(predictors) == 0){
    imp <- cbind(by[[1]], medians) # about 75 times faster than merge
  } else {
    imp <- dat[predictors]
    imp$..._order_... <- seq_len(nrow(dat))
    imp <- merge(imp, medians, all.x=TRUE, all.y=FALSE, sort=FALSE)
    imp <- imp[order(imp$..._order_...),,drop=FALSE]
  } 
  
  for ( p in predicted ){
    if (is.na(medians[1,p]))
      warning(sprintf("Could not compute predictor for %s, imputing NA",p))
    ina <- is.na(dat[p])
    dat[ina,p] <- imp[ina,p] 
    + get_res(nmiss=sum(ina), residuals=imp[!ina,p]-dat[!ina,p], type=add_residual)
  }
  dat 
}