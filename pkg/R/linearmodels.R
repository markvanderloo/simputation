
#' (Robust) Linear Regression Imputation
#'
#' Regression imputation methods including linear regression, robust 
#' linear regression with \eqn{M}-estimators, regularized regression
#' with lasso/elasticnet/ridge regression.
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
#'  
#' @param na_action \code{[function]} what to do with missings in training data.
#'   By default cases with missing values in predicted or predictors are omitted
#'   (see `Missings in training data').
#' @param ... further arguments passed to 
#' \itemize{
#' \item{\code{\link[stats]{lm}} for \code{impute_lm}}
#' \item{\code{\link[MASS]{rlm}} for \code{impute_rlm}}
#' \item{\code{\link[glmnet]{glmnet}} for \code{impute_en}}
#' }
#' 
#' @section Model specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. The right-hand side excluding the optional \code{GROUPING_VARIABLES} 
#' model specification for the underlying predictor.
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
#' Grouping is ignored for \code{impute_const}.
#' 
#' @section Methodology:
#' 
#' \bold{Linear regression model imputation} with \code{impute_lm} can be used 
#' to impute numerical variables based on numerical and/or categorical 
#' predictors. Several common imputation methods, including ratio and (group)
#' mean imputation can be expressed this way. See \code{\link[stats]{lm}} for
#' details on possible model specification.
#' 
#' \bold{Robust linear regression through M-estimation} with
#' \code{impute_rlm} can be used to impute numerical variables employing
#' numerical and/or categorical predictors. In \eqn{M}-estimation, the
#' minimization of the squares of residuals is replaced with an alternative
#' convex function of the residuals. A concise online description
#' of \eqn{M}-estimation can be found 
#' \href{http://research.microsoft.com/en-us/um/people/zhang/INRIA/Publis/Tutorial-Estim/node24.html#SECTION000104000000000000000}{here}.
#' Also see e.g.  Huber (1981).
#' 
#' \bold{Lasso/elastic net/ridge regression imputation} with \code{impute_en} 
#' can be used to impute numerical variables employing numerical and/or 
#' categorical predictors. For this method, the regression coefficients are 
#' found by minimizing the least sum of squares of residuals augmented with a 
#' penalty term depending on the size of the coefficients. For lasso regression 
#' (Tibshirani, 1996), the penalty term is the sum of squares of the 
#' coefficients. For ridge regression (Hoerl and Kennard, 1970), the penalty
#' term is the sum of absolute values of the coefficients. Elasticnet regression
#' (Zou and Hastie, 2010) allows switching from lasso to ridge by penalizing by
#' a weighted sum of the sum-of-squares and sum of absolute values term.
#' 
#'
#' @references 
#' Huber, P.J., 2011. Robust statistics (pp. 1248-1251). Springer Berlin Heidelberg.
#'
#' Hoerl, A.E. and Kennard, R.W., 1970. Ridge regression: Biased estimation for
#' nonorthogonal problems. Technometrics, 12(1), pp.55-67.
#'
#' Tibshirani, R., 1996. Regression shrinkage and selection via the lasso.
#' Journal of the Royal Statistical Society. Series B (Methodological),
#' pp.267-288.
#'   
#' Zou, H. and Hastie, T., 2005. Regularization and variable selection via the
#' elastic net. Journal of the Royal Statistical Society: Series B (Statistical
#' Methodology), 67(2), pp.301-320.
#' 
#' @seealso 
#' \href{../doc/intro.html}{Getting started with simputation}, 
#' 
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
#' @rdname impute_lm
#' @export
impute_lm <- function(dat, formula, add_residual = c("none","observed","normal")
                      ,na_action=na.omit, ...){
    add_residual <- match.arg(add_residual)
    do_by(dat, groups(dat,formula), .fun=lmwork
          , formula=remove_groups(formula), add_residual=add_residual, fun=lm
          , na.action=na_action, ...)
}


#' @rdname impute_lm
#' @export
impute_rlm <- function(dat, formula, add_residual = c("none","observed","normal"), na_action=na.omit,...){
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=lmwork
    , formula=remove_groups(formula), add_residual=add_residual, MASS::rlm
    , na.action=na_action, ...)
}



#' @rdname impute_lm
#' 
#' @param family Response type for elasticnet / lasso regression. For 
#' \code{family="gaussian"} the imputed variables are general numeric variables.
#' For \code{family="poisson"} the imputed variables are nonnegative counts.
#' See \code{\link[glmnet:glmnet]{glmnet}} for details.
#' @param s The value of \eqn{\lambda} to use when computing predictions for 
#'   lasso/elasticnet regression (parameter \var{s} of 
#'   \code{\link[glmnet:predict.glmnet]{predict.glmnet}}). For \code{impute\_en} the (optional)
#'   parameter \var{lambda} is passed to \code{\link[glmnet]{glmnet}} when estimating
#'   the model (which is advised against).
#'   
#' 
#' @export
impute_en <- function(dat, formula
      , add_residual = c("none","observed","normal")
      , na_action=na.omit, family=c("gaussian","poisson"), s = 0.01, ...){

  if (not_installed("glmnet")) return(dat)
    
  family <- match.arg(family)
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=lmwork
        , formula=remove_groups(formula)
        , add_residual=add_residual, fun=fdglmnet
        , na.action=na_action, family=family, s=s, ...)
  
}


predict.simputation.glmnet <- function(object, newdat, ...){
  tm <- terms(object$formula)
  
  # only complete cases in predictors can be used to compute imputations.
  vars <- attr(tm,"term.labels")
  cc <- complete.cases(newdat[vars])
  y <- rep(NA_real_, nrow(newdat))
  if (!any(cc)) return(y)
  
  newx <- model.matrix(stats::delete.response(tm),newdat)
  
  responsetype <- c(gaussian="link",poisson = "response")
  type <- responsetype[object$family]
  y[cc] <- glmnet::predict.glmnet(object, newx=newx, type=type, s=object$s, ...)
  y
}

residuals.simputation.glmnet <- function(object,...){
  object$residuals
}

has_intercept <- function(frm){
  attr(terms(frm),"intercept") == 1
}

# formula-data interface to glmnet::glmnet, single numerical predicted variable
fdglmnet <- function(formula, data, na.action=na.omit, family, s, ...)
{
  dat <- na.action(data)
  
  intercept <- FALSE
  if (has_intercept(formula)){
    formula <- update.formula(formula, . ~ . -1)
    intercept <- TRUE
  }
  
  x <- stats::model.matrix(formula, data=dat)
  if (dim(x)[2] <= 1){
    warnf("glmnet expects at least two predictors. Returning original data")
    return(data)
  }
  y <- eval(formula[[2]],envir = dat)
  m <- glmnet::glmnet(x=x,y=y,intercept=intercept,family=family,...)
  # store extra info for the predictor.
  m$formula <- formula
  m$family <- family
  m$s <- s
  m$residuals <- y - glmnet::predict.glmnet(m, newx=x, s=s)
  class(m) <- c("simputation.glmnet",class(m))
  m
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










get_res <- function(nmiss, residuals, type){
  switch(type
     , none = rep(0,nmiss)
     , observed = sample(x = residuals, size=nmiss, replace=TRUE)
     , normal = rnorm(n=nmiss, mean=mean(residuals), sd=sd(residuals))
  )
}



