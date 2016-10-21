
#' Impute missing data
#'
#' Use to fit and impute missing data.
#'
#' @param dat \code{[data.frame]}, with variables to be imputed and their
#'   predictors.
#' @param formula \code{[formula]} imputation model description (see Details below).
#' @param add_residual \code{[character]} Type of residual to add. \code{"normal"} 
#'   means that the imputed value is drawn from \code{N(mu,sd)} where \code{mu}
#'   and \code{sd} are estimated from the model's residuals (\code{mu} should equal
#'   zero in most cases). If \code{add_residual = "observed"}, residuals are drawn
#'   (with replacement) from the model's residuals. Ignored for non-numeric 
#'   predicted variables.
#'  
#' @param na.action \code{[function]} what to do with missings in training data.
#'   By default cases with missing values in predicted or predictors are omitted
#'   (see `Missings in training data').
#' @param ... further arguments passed to 
#' \itemize{
#' \item{\code{\link[stats]{lm}} for \code{impute_lm}}
#' \item{\code{\link[MASS]{rlm}} for \code{impute_rlm}}
#' \item{\code{\link[base]{order}} for \code{impute_shd}} 
#' \item{The \code{predictor} for \code{impute_pmm}}
#' \item{\code{\link[randomForest]{randomForest}} for \code{impute_rf}}
#' }
#' 
#' @section Specifying the imputation model:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. The interpretation of the independent variables on the
#' right-hand-side depends on the underlying imputation model. If grouping
#' variables are specified, the data set is split according to the values of
#' those variables, and model estimation and imputation occur independently for
#' each group.
#' 
#' 
#' Grouping using \code{dplyr::group_by} is also supported. If groups are 
#' defined in both the formula and using \code{dplyr::group_by}, the data is 
#' grouped by the union of grouping variables. Any missing value in one of the
#' grouping variables results in an error.
#' 
#' Grouping is ignored for \code{impute_const}.
#' 
#' @section Details:
#' 
#' The functions are designed to be robust against failing imputations. This means that
#' rather than emitting an error, functions show the following behaviour.
#' 
#' \itemize{
#' \item{If a value cannot be imputed because one of its predictors is missing, the value will
#' remain missing after imputation.}
#' 
#' \item{If a model cannot be fitted, e.g. because the imputed model is missing, a warning
#' is emitted and for that variable no imputation will take place.}
#' }
#'
#' @section Missings in training data:
#' 
#' For model-based imputation, including those based on (robust) linear models, 
#' cart models and random forests, there is an option called \code{na.action}
#' that specifies what to do with missings in training data. The default action
#' is to train models on data where both the predicted and predictor variables
#' are available. Some of the interesting options are
#' 
#' \itemize{
#' \item{\code{\link[stats]{na.omit}}: omit cases where predictor or predicted
#'   is missing. This is the default.} 
#'   \item{\code{rpart::\link[rpart]{na.rpart}}: omit cases where the predicted is
#'   missing but keep cases where one or more predictors are missing. Relevant
#'   for \code{impute_cart}}
#' \item{\code{randomForest::\link[randomForest]{na.roughfix}} Temporarily impute all
#'   predictors and predicted with the column median (for numeric data) or the
#'   mode (for categorical data) in order to fit the model. }
#' }
#' 
#' 
#' 
#' 
#'
#'
#'  
#'  
#'  
#'    
#' 
#' @section Model descriptions:
#' 
#' \tabular{ll}{
#' \bold{Model} \tab \bold{description}\cr
#' \code{impute_lm} \tab Use \code{stats::lm} to train the imputation model.\cr
#' \code{impute_rlm} \tab Use \code{MASS::rlm} to train the imputation model.\cr
#' \code{impute_median} \tab Median imputation. Predictors are treated
#'    as grouping variables for computing medians.\cr
#' \code{impute_const} \tab Impute a constant value \cr
#' \code{impute_proxy} \tab Copy a value from the predictor variable.\cr
#' \code{impute_rhd} \tab Random hot deck. Predictors are used to group the donors.\cr
#' \code{impute_shd} \tab Sequential hot deck. Predictors sort the data (use \code{~ 1} for no sorting).\cr
#' \code{impute_knn} \tab k-nearest neighbour imputation. Predictors are used to determine Gower's distance.\cr
#' \code{impute_pmm} \tab Predictive mean matching. \cr
#' \code{impute_cart} \tab Use \code{rpart::rpart} to train a CART model.\cr
#' \code{impute_rf} \tab Use \code{randomForest::randomForest} to train the predictive model.
#' }
#'
#' @seealso 
#' \href{../doc/intro.html}{Getting started with simputation}, 
#' \code{\link[stats]{lm}} \code{\link[MASS]{rlm}} \code{\link[rpart]{rpart}}
#' 
#'
#' @return \code{dat}, but imputed where possible.
#' 
#' 
#' @examples
#' 
#' data(iris)
#' irisNA <- iris
#' irisNA[1:4, "Sepal.Length"] <- NA
#' irisNA[3:7, "Sepal.Width"] <- NA
#' 
#' # impute a single variable (Sepal.Length)
#' i1 <- impute_lm(irisNA, Sepal.Length ~ Sepal.Width + Species)
#' 
#' # impute both Sepal.Length and Sepal.Width, using robust linear regression
#' i2 <- impute_rlm(irisNA, Sepal.Length + Sepal.Width ~ Species + Petal.Length)
#' 
#' @name impute_
#' @rdname impute_ 
#' @export
impute_lm <- function(dat, formula, add_residual = c("none","observed","normal")
                      ,na.action=na.omit, ...){
    add_residual <- match.arg(add_residual)
    do_by(dat, groups(dat,formula), .fun=lmwork
          , formula=remove_groups(formula), add_residual=add_residual, fun=lm, na.action=na.action, ...)
}


#' @rdname impute_
#' @export
impute_rlm <- function(dat, formula, add_residual = c("none","observed","normal"), na.action=na.omit,...){
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=lmwork
    , formula=remove_groups(formula), add_residual=add_residual, MASS::rlm, na.action=na.action, ...)
}

lmwork <- function(dat, formula, add_residual, fun, na.action, ...){
  stopifnot(inherits(formula,"formula"))

  predicted <- get_imputed(formula, dat)
  predicted <- predicted[sapply(dat[predicted], is.numeric)]
  formulas <- paste(predicted, "~" ,deparse(formula[[3]]) )
  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(dat[,p])
    nmiss <- sum(ina)
    if (!any(ina) ) next # skip if no missings
    m <- run_model(fun, formula=as.formula(formulas[i]), data=dat,na.action=na.action,...)
    res <- get_res(nmiss = sum(ina), residuals = residuals(m), type = add_residual)
    dat[ina, p] <- stats::predict(m, newdat = dat[ina,,drop=FALSE]) + res
  }
  dat
  
}


#' @rdname impute_
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



#' @rdname impute_
#' @export
impute_median <- function(dat, formula, add_residual = c("none","observed","normal"), ...){
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
    stats::aggregate(dat[predicted],by=by, FUN=stats::median, na.rm=TRUE)
    , warning=function(w) invokeRestart("muffleWarning") 
  )
  # create nrow(data) X npredictors data.frame with formula values.
  imp <- if (length(predictors) == 0) 
    cbind(by[[1]], medians) # about 75 times faster than merge
  else 
    merge(dat[predictors], medians, all.x=TRUE, all.y=FALSE)
  
  for ( p in predicted ){
    if (is.na(medians[1,p]))
      warning(sprintf("Could not compute predictor for %s, imputing NA",p))
    ina <- is.na(dat[p])
    dat[ina,p] <- imp[ina,p] + get_res(nmiss=sum(ina), residuals=imp[!ina,p]-dat[!ina,p], type=add_residual)
  }
  dat
}





#' @rdname impute_
#' @export
impute_proxy <- function(dat, formula, add_residual = c("none","observed","normal"), ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  formula <- remove_groups(formula)
  predicted <- get_imputed(formula,names(dat))
  predictor <- get_predictors(formula, names(dat))
  if( length(predictor) != 1 )
    stop(sprintf("Need precisely one predictor, got %d",length(predictor)) )
  
  for ( p in predicted){
    ina <- is.na(dat[,p])
    dat[ina,p] <- dat[ina,predictor] + 
      get_res(sum(ina), dat[!ina,predictor] - dat[!ina,p], type=add_residual)
  }
  dat
}




get_res <- function(nmiss, residuals, type){
  switch(type
     , none = rep(0,nmiss)
     , observed = sample(x = residuals, size=nmiss, replace=TRUE)
     , normal = rnorm(n=nmiss, mean=mean(residuals), sd=sd(residuals))
  )
}



