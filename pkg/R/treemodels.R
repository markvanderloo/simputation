

#' @rdname impute_
#' @param cp The complexity parameter used to \code{\link[rpart]{prune}} the CART model. If
#'    omitted, no pruning takes place. If a single number, the same complexity parameter is
#'    used for each imputed variable. If of length \code{#} of variables imputed, the complexity
#'    parameters used must be in the same order as the predicted variables in the \code{model}
#'    formula.
#'    
#' @export
impute_cart <- function(dat, formula, add_residual=c("none","observed","normal"), cp,
                        na.action=na.omit, ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=cart_work
        , formula=remove_groups(formula)
        , add_residual=add_residual
        , cp , na.action=na.action, ...)
}

cart_work <- function(dat, formula, add_residual, cp, na.action, ...){

  predicted <- get_imputed(formula, dat)
  formulas <- paste(predicted, "~" ,deparse(formula[[3]]) )

  cp <- if (missing(cp)){
    rep(0,length(predicted))
  } else {
    if (length(cp)==1){ 
      rep(cp,length(predicted))
    } else if(length(cp) != length(predicted)) { 
      stop(sprintf("Length of cp (%d) not equal to number of imputed variables (%d)"
                   ,length(predicted),length(cp)), call.=FALSE) 
    } else {
      cp
    }
  }
  names(cp) <- predicted
  for (i in seq_along(predicted)){
    p <- predicted[i]
    ina <- is.na(dat[,p])
    m <- run_model(rpart, formula = as.formula(formulas[i]), data=dat, na.action=na.action,...)
    m <- rpart::prune(m,cp[p])
    if (is.numeric(dat[,p])){
      res <- get_res(nmiss = sum(ina),residuals = residuals(m), type=add_residual)
      dat[ina,p] <- predict(m,dat[ina,,drop=FALSE]) + res
    } else if (is.logical(dat[,p])){
      dat[ina,p] <- as.logical(predict(m, dat[ina,,drop=FALSE]))
    } else { 
      dat[ina,p] <- predict(m, dat[ina,,drop=FALSE],type="class")
    }
  }
  dat
}


#' @rdname impute_
#' 
#' @export
impute_rf <- function(dat, formula, add_residual = c("none","observed","normal")
                      , na.action=na.omit, ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=rf_work
    , formula=remove_groups(formula), add_residual=add_residual, na.action=na.action,...)
}

rf_work <- function(dat, formula, add_residual = c("none","observed","normal"), na.action, ...){
  stopifnot(inherits(formula,"formula"))
  
  predictors <- get_predictors(formula, dat)
  predicted <- get_imputed(formula, dat)
  formulas <- paste(predicted, "~" ,deparse(formula[[3]]) )
  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    cc <- complete.cases(dat[c(p,predictors)])
    m <- run_model(randomForest::randomForest
           , formula=as.formula(formulas[i])
           , data=dat, na.action=na.action, ...)
    ina <- is.na(dat[,p])
    dat[ina,p] <- predict(m, newdata=dat[ina,,drop=FALSE])
    if (is.numeric(dat[,p]) && add_residual != "none"){
      res <- get_res(nmiss=sum(ina), residuals = dat[cc,p]-predict(m), type=add_residual)
      dat[ina,p] <- dat[ina,p] + res
    }
  }
  dat
}








