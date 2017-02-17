
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
  predict(object, newdata, type=type, ...)
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

#' @export
#' @rdname foretell
foretell.glmnet <- function(object, newdata, ...){
  tm <- terms(object$formula)
  
  # only complete cases in predictors can be used to compute predictions.
  vars <- attr(tm,"term.labels")
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
  type <- responsetype[object$family]
  y[cc] <- predict(object, newx=newx, type=type, s=object$s, ...)
  y
}


