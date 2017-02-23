
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
  type <- match.arg(type,responsetype)
  out <- predict(object, newdata=newdata, type=type, ...)
  if (type == "matrix") out <- out[,2]
  if (type == "class" && is_logical(out)) out <- as.logical(as.character(out))
  out
}

# Check if we're dealing with a factor that is actually a logical.
is_logical <- function(x){
  is.factor(x) && nlevels(x) == 2 &&  all(levels(x) %in% c("FALSE","TRUE")) 
}
  




