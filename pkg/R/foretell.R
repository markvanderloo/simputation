
#' Alternative to 'predict' returning values of correct type.
#'
#' Te default \code{precict} function doesn't always return the
#' predicted variable by default. For example, when estimating
#' a binomial model using \code{\link[stats]{glm}}, by default the
#' log-odds are returned. \code{foretell} wraps \code{predict} while
#' setting options so that the actual predicted value is returned.
#' 
#' @param object A model object,(\code{lm}, \code{glm}, ...)
#' @param ... Furher arguments passed to \code{predict}.
#' @param newdata \code{[data.frame]} a data frame in which to look
#'   for variables with which to predict. For \code{glmnet} models
#'   this argument is mandatory.
#' @param type \code{[character]} Type of output. If missing, the type of
#' predicted variable is returned.
#' 
#' @export
foretell <- function(object,...){
  UseMethod("foretell")
}

#' @export 
#' @rdname foretell
foretell.default <- function(object,...) predict(object,...)

#' @export
#' @rdname foretell
foretell.glm <- function(object, newdata=NULL, type, ...){
  if (missing(type)) type <- "response"
  family <- object$family$family
  p <- predict(object, newdata, type=type, ...)
  if (family == "binomial") p > 0.5 else p
}

#' @export
#' @rdname foretell
foretell.rpart <- function(object, newdata, type,...){
  responsetype <- c(
    "anova" = "vector"
    , "class" = "class"
    , "poisson" = "matrix"
    , "exp" = "vector"
  )
  if (missing(type) ){
    type <- responsetype[object$method]
  } 
  out <- predict(object, newdata=newdata, type=type, ...)
  if (type == "matrix") out[,2] else out
}

#' @rdname foretell
#' @param s The value of \eqn{\lambda} to use when computing predictions for 
#'   lasso/elasticnet regression (parameter \var{s} of 
#'   \code{\link[glmnet:predict.glmnet]{predict.glmnet}}).
#' @export
foretell.glmnet <- function(object, newdata, type, s=object$s, ...){
  vars <- rownames(object$beta)
  # only complete cases in predictors can be used to compute predictions.
  cc <- complete.cases(newdata[vars])
  y <- rep(NA_real_, nrow(newdata))
  if (!any(cc)) return(y)
  
  newx <- model.matrix(stats::delete.response(tm),newdata)
  
  responsetype <- c(
    gaussian="link"
    , binomial = "class"
    , poisson = "response"
    , multinomial = "class"
    , cox = "response"
    , mgaussian = "response")
  
  
  get_glm_family <- function(m){
    # get textversion of call
    str <- deparse(m$call, width.cutoff=500)
    # remove possible line-breaks
    str <- gsub("\\n+"," ",str)
    # Check whether a 'family' argument was applied. This is safe because both 
    # partial matches ('fam') and match-by-order get expanded before the call is
    # stored in the 'glm' object.
    has_family <- grepl("family[[:blank:]]*=",str)
    family <- if (!has_family){
      return("gaussian") # the default
    } else {
      sub(".*family[[:blank:]]*=[[:blank:]]*\"(.+?)\"[,)]","\\1",str)
    }
    # if the family name was abbreviated, we'll still find it...
    lookup <- names(responsetype)
    family <- lookup[pmatch(family,lookup)]
    # ...unless we don't (but that should in principle not happen)
    if(is.na(out)) warnf("could not determine glmnet family argument")
    out
  }
  if (missing(type)){
    family <- get_glm_family(object)
    type <- responsetype["family"]
  }
  y[cc] <- predict(object, newx=newx, type=type, s=s, ...)
  y
}


  
  
  




