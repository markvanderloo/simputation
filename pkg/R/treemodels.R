

#' Decision Tree Imputation
#' 
#' Imputation based on CART models or Random Forests.
#' 
#' @param dat \code{[data.frame]}, with variables to be imputed and their
#'   predictors.
#' @param formula \code{[formula]} imputation model description (see Details below).
#' @param cp The complexity parameter used to \code{\link[rpart]{prune}} the CART model. If
#'    omitted, no pruning takes place. If a single number, the same complexity parameter is
#'    used for each imputed variable. If of length \code{#} of variables imputed, the complexity
#'    parameters used must be in the same order as the predicted variables in the \code{model}
#'    formula.
#' @param add_residual \code{[character]} Type of residual to add. \code{"normal"} 
#'   means that the imputed value is drawn from \code{N(mu,sd)} where \code{mu}
#'   and \code{sd} are estimated from the model's residuals (\code{mu} should equal
#'   zero in most cases). If \code{add_residual = "observed"}, residuals are drawn
#'   (with replacement) from the model's residuals. Ignored for non-numeric 
#'   predicted variables.
#' @param na_action \code{[function]} what to do with missings in training data.
#'   By default cases with missing values in predicted or predictors are omitted
#'   (see `Missings in training data').
#' @param ... further arguments passed to 
#' \itemize{
#' \item{\code{\link[rpart]{rpart}} for \code{impute_cart}}
#' \item{\code{\link[randomForest]{randomForest}} for \code{impute_rf}}
#' \item{\code{\link[missForest]{missForest}} for \code{impute_mf}}
#' }
#' @rdname impute_tree   
#' @export
impute_cart <- function(dat, formula, add_residual=c("none","observed","normal"), cp,
                        na_action=na.rpart, ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=cart_work
        , formula=remove_groups(formula)
        , add_residual=add_residual
        , cp , na_action=na_action, ...)
}

cart_work <- function(dat, formula, add_residual, cp, na_action, ...){

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
    m <- run_model(rpart, formula = as.formula(formulas[i]), data=dat, na.action=na_action,...)
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


#' @rdname impute_tree
#' 
#' @export
impute_rf <- function(dat, formula, add_residual = c("none","observed","normal")
                      , na_action=na.omit
                      , ...){
  if (not_installed("randomForest")) return(dat)
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=rf_work
    , formula=remove_groups(formula), add_residual=add_residual, na_action=na_action,...)
}

rf_work <- function(dat, formula, add_residual = c("none","observed","normal"), na_action, ...){
  stopifnot(inherits(formula,"formula"))
  
  predictors <- get_predictors(formula, dat)
  predicted <- get_imputed(formula, dat)
  # we need to work around a formula-handling bug in 'randomForest <= 4.6-12' 
  # (the ". - x" case is not handled correctly)
  # The following should work when randomForest gets updated.
  # formulas <- paste(predicted, "~" ,deparse(formula[[3]]) )
  
  for ( i in seq_along(predicted) ){
    p <- predicted[i]
    cc <- complete.cases(dat[c(p,predictors)])
    ipredictors <- setdiff(predictors, predicted[i])
    frm <- paste(predicted[i], " ~ ",paste(ipredictors,collapse=" + ") )
    m <- run_model(randomForest::randomForest
           , formula=as.formula(frm)
           , data=dat, na.action=na_action, ...)
    ina <- is.na(dat[,p])
    dat[ina,p] <- predict(m, newdata=dat[ina,,drop=FALSE])
    if (is.numeric(dat[,p]) && add_residual != "none"){
      res <- get_res(nmiss=sum(ina), residuals = dat[cc,p]-predict(m), type=add_residual)
      dat[ina,p] <- dat[ina,p] + res
    }
  }
  dat
}

#' @rdname impute_tree
#' 
#' @export
impute_mf <- function(dat, formula,...){
  stopifnot(inherits(formula,"formula"))
  if ( not_installed("missForest") ) return(dat)
  imputed <- get_imputed(formula,dat)
  predictors <- get_predictors(formula, dat)
  vars <- unique(c(imputed,predictors))
  imp <- tryCatch(missForest::missForest(dat[vars])[[1]], error=function(e){
    warnf("Could not execute missForest::missForest: %s\n Returning original data"
         , e$message)
    dat
  })
  dat[imputed] <- imp[imputed]
  dat
}





