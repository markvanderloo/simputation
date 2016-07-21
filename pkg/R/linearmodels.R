
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
  predicted <- get_predicted(x, names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
    formulas <- paste(predicted, "~" ,deparse(x[[3]]) )  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(data[,p])
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
  predicted <- get_predicted(x,names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
  formulas <- paste(predicted, "~" ,deparse(x[[3]]) )  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(data[,p])
    if (!any(ina)) next # skip if no missings
    m <- MASS::rlm(as.formula(formulas[i]), data=data, ...)
    data[ina, p] <- stats::predict(m, newdata=data[ina,,drop=FALSE])
  }
  data
}


#' @rdname impute_lm
impute_const <- function(data, x, ...){
  stopifnot(inherits(x,"formula"))
  if (length(x[[3]]) != 1)
    stop(sprintf("Expected constant, got '%s'",deparse(x[[3]])))
  const <- as.numeric(deparse(x[[3]]))
  if (is.na(const)) const <- deparse(x[[3]])
  predicted <- get_predicted(x,names(data))
  for ( p in predicted ){
    ina <- is.na(data[p])
    # prevent conversion to NA from popping up 
    # (we replace NA with NA in that case) 
    tryCatch(
      data[ina,p] <- const,silent=TRUE
             , warning=function(w){}
      )
  }
  data
}



#' @rdname impute_lm
#' @export
impute_median <- function(data, x, ...){
  impute_median_base(data,x,...)
  # TODO: conditional on presence of dplyr, use summarise_
}


impute_median_base <- function(data,x,...){
  predicted <- get_predicted(x,names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
  predictors <- get_predictors(x,names(data))
  if (length(predictors) == 0){
    for (p in predicted){
      ina <- is.na(data[p])
      data[ina,p] <- stats::median(data[,p],na.rm=TRUE)
    }
    return(data)
  }
  by <- as.list(data[predictors])
  medians <- stats::aggregate(data[predicted],by=by, FUN=stats::median, na.rm=TRUE)
  imp <- merge(data[predictors],medians,all.x=TRUE,all.y=FALSE)
  for ( p in predicted ){
    ina <- is.na(data[p])
    data[ina,p] <- imp[ina,p]
  }
  data
}





get_predictors <- function(frm, vars){
  v <- all.vars(frm[[3]])
  w <-v[!v %in% vars]
  if( length(w) > 0)
    stop(sprintf("Predictors %s not found in data",paste(w,collapse=", ")))
  v
}




# frm: a formula
# vars: variable names
get_predicted <- function(frm, vars){
  v <- all.vars(frm[[2]])
  w <- all.vars(frm[[3]])
  if ( identical(v , ".") ){ 
    v <- vars
    v <- setdiff(v,w)
  }
  if (any(v %in% w))
    stop(sprintf("Using '%s' as predictor and predicted"
                  , paste(v,collapse=", ")), call.=FALSE)
  w <- v[!v %in% vars]
  if (length(w)>0)
    stop(sprintf("Trying to impute variables not in data: %s"
                 ,paste(w,collapse=", ")), call.=FALSE)
  # TODO: allow '.' and minus signs
  v
}




# TODO:
# impute_median
# impute_const
# ...
