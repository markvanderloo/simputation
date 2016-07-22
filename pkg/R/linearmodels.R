
#' Impute missing data
#'
#' Use to fit and impute missing data.
#'
#' @param data The data
#' @param model a \code{\link[stats]{formula}} object.
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
#' irisNA <- iris
#' irisNA[1:4, "Sepal.Length"] <- NA
#' irisNA[3:7, "Sepal.Width"] <- NA
#' 
#' # impute a single variable (Sepal.Length)
#' i1 <- impute_lm(irisNA, Sepal.Length ~ Sepal.Width + Species)
#' 
#' # impute both Sepal.Length and Sepal.Width, using robust imputation
#' i2 <- impute_lm(irisNA, Sepal.Length + Sepal.Width ~ Species + Petal.Length)
#' 
#' @name impute_
#' @rdname impute_ 
#' @export
impute_lm <- function(data, model, add_residual = c("none","observed","normal"), ...){
  add_residual <- match.arg(add_residual)
  lmwork(data=data, model=model, add_residual=add_residual, fun=stats::lm, ...)  
}

#' @rdname impute_
impute_rlm <- function(data, model, add_residual = c("none","observed","normal"), ...){
  add_residual <- match.arg(add_residual)
  lmwork(data=data, model=model, add_residual=add_residual, fun=MASS::rlm, ...)  
}

lmwork <- function(data, model, add_residual, fun, ...){
  stopifnot(inherits(model,"formula"))

  predicted <- get_predicted(model, names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
  formulas <- paste(predicted, "~" ,deparse(model[[3]]) )
  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    ina <- is.na(data[,p])
    nmiss <- sum(ina)
    if (!any(ina)) next # skip if no missings
    m <- fun(as.formula(formulas[i]), data=data, ...)
    res <- switch(add_residual
                  , none = rep(0, nmiss)
                  , observed = sample(residuals(m),size = nmiss, replace=TRUE)
                  , normal = rnorm(n=nmiss, mean=0, sd=sd(residuals(m)))
                )
    data[ina, p] <- stats::predict(m, newdata=data[ina,,drop=FALSE]) + res
  }
  data
  
}




#' @rdname impute_
impute_const <- function(data, model, ...){
  stopifnot(inherits(model,"formula"))
  if (length(model[[3]]) != 1)
    stop(sprintf("Emodelpected constant, got '%s'",deparse(model[[3]])))
  const <- as.numeric(deparse(model[[3]]))
  if (is.na(const)) const <- deparse(model[[3]])
  predicted <- get_predicted(model,names(data))
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



#' @rdname impute_
#' @export
impute_median <- function(data, model, ...){
  impute_median_base(data,model,...)
  # TODO: conditional on presence of dplyr, use summarise_
}


impute_median_base <- function(data,model,...){
  predicted <- get_predicted(model,names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
  predictors <- get_predictors(model,names(data))
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

#' @rdname impute_
impute_proxy <- function(data, model, ...){
  predicted <- get_predicted(model,names(data))
  predictor <- get_predictors(model, names(data))
  if( length(predictor) != 1 )
    stop(sprintf("Need precisely one predictor, got %d",length(predictor)) )
  
  for ( p in predicted){
    ina <- is.na(data[,p])
    data[ina,p] <- data[ina,predictor]
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




