
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
    if (!any(ina) ) next # skip if no missings
    m <- tryCatch(fun(as.formula(formulas[i]), data=data, ...)
          , error=function(e){
            warning(sprintf("Could not fit model for '%s' because %s\n",p,e$message),call.=FALSE)
            structure(NA,class="dummymodel")
          })
    res <- get_res(nmiss = sum(ina), residuals = residuals(m), type = add_residual)
    data[ina, p] <- stats::predict(m, newdata = data[ina,,drop=FALSE]) + res
  }
  data
  
}

predict.dummymodel <- function(object,...) NA
residuals.dummymodel <- function(object,...) NA

#' @rdname impute_
impute_const <- function(data, model, add_residual = c("none","observed","normal"),...){
  stopifnot(inherits(model,"formula"))
  add_residual <- match.arg(add_residual)
  
  if (length(model[[3]]) != 1)
    stop(sprintf("Emodelpected constant, got '%s'",deparse(model[[3]])))
  const <- as.numeric(deparse(model[[3]]))
  
  if (is.na(const)) const <- deparse(model[[3]])
  predicted <- get_predicted(model,names(data))
  for ( p in predicted ){
    ina <- is.na(data[p])
    nmiss <- sum(ina)
    # prevent conversion of constant to NA from popping up: we just replace NA 
    # with NA in that case.
    tryCatch(
      data[ina,p] <- if ( add_residual == "none" ){
        const
      } else if (nmiss == 0){
        warning("All values missing, so no random residuals added")
        const
      } else {
        const + get_res(nmiss=nmiss, residuals=const-data[!ina,p], type=add_residual)
      }
      , warning=function(w){}
    )
  }
  data
}


get_res <- function(nmiss, residuals, type){
  switch(type
    , none = rep(0,nmiss)
    , observed = sample(x = residuals, size=nmiss, replace=TRUE)
    , normal = rnorm(n=nmiss, mean=mean(residuals), sd=sd(residuals))
  )
}


#' @rdname impute_
#' @export
impute_median <- function(data, model, add_residual = c("none","observed","normal"), ...){
  stopifnot(inherits(model,"formula"))
  add_residual <- match.arg(add_residual)
  
  predicted <- get_predicted(model,names(data))
  predicted <- predicted[sapply(data[predicted], is.numeric)]
  predictors <- get_predictors(model,names(data))
  
  # compute model values
  by <- if (length(predictors) == 0) list(rep(1,nrow(data))) else as.list(data[predictors])
  # silence the warning about producing NA's (give specific warning later)
  medians <- withCallingHandlers(
    stats::aggregate(data[predicted],by=by, FUN=stats::median, na.rm=TRUE)
    , warning=function(w) invokeRestart("muffleWarning") 
  )
  # create nrow(data) X npredictors data.frame with model values.
  imp <- if (length(predictors) == 0) 
    cbind(by[[1]], medians) # about 75 times faster than merge
  else 
    merge(data[predictors], medians, all.x=TRUE, all.y=FALSE)
  
  for ( p in predicted ){
    if (is.na(medians[1,p]))
      warning(sprintf("Could not compute predictor for %s, imputing NA",p))
    ina <- is.na(data[p])
    data[ina,p] <- imp[ina,p] + get_res(nmiss=sum(ina), residuals=imp[!ina,p]-data[!ina,p], type=add_residual)
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




