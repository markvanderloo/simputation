

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
#' @param impute_all \code{[logical]} If FALSE (default) then only missings in
#'   predicted variables are imputed. If TRUE, predictions are imputed for all
#'   records and if a prediction cannot be made then NA is imputed.
#' @param ... further arguments passed to 
#' \itemize{
#' \item{\code{\link[rpart]{rpart}} for \code{impute_cart}}
#' \item{\code{\link[randomForest]{randomForest}} for \code{impute_rf}}
#' }
#' 
#' @section Model specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. Variables on the right-hand-side are used as predictors in the
#' CART or random forest model.
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
#'
#' @section Methodology:
#' 
#' \bold{CART imputation} by \code{impute_cart} can be used for numerical,
#' categorical, or mixed data. Missing values are estimated using a 
#' Classification and Regression Tree as specified by Breiman, Friedman and
#' Olshen (1984). This means that prediction is fairly robust agains missingess
#' in predictors.
#' 
#' \bold{Random Forest imputation} with \code{impute_rf} can be used for numerical,
#' categorical, or mixed data. Missing values are estimated using a Random Forest
#' model as specified by Breiman (2001).
#' 
#' 
#' @references 
#' 
#' Breiman, L., Friedman, J., Stone, C.J. and Olshen, R.A., 1984. Classification
#' and regression trees. CRC press.
#'   
#' Breiman, L., 2001. Random forests. Machine learning, 45(1), pp.5-32.
#'
#' @rdname impute_tree   
#' @family imputation
#' @export
impute_cart <- function(dat, formula, add_residual=c("none","observed","normal"), cp,
                        na_action=na.rpart, impute_all=FALSE, ...){
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=cart_work
        , formula=remove_groups(formula)
        , add_residual=add_residual
        , cp, na_action=na_action, impute_all=impute_all, ...)
}

cart_work <- function(dat, formula, add_residual, cp, na_action, impute_all, ...){

  predicted <- get_imputed(formula, dat)

  # Set complexity parameter based on function argument.
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

  # Iterate over target variables to impute each of them.
  for (p in predicted) {
    # Get logical vector for rows to impute.
    i <- if (impute_all) rep(TRUE, nrow(dat)) else is.na(dat[, p])
    if (!any(i)) {
      next  # Skip if nothing to impute.
    }

    # Build model for p and use it to impute dat[i, p].
    p_formula <- as.formula(paste(p, "~" , deparse(formula[[3]])))
    m <- run_model(rpart, formula = p_formula, data = dat, na.action = na_action, ...)
    m <- rpart::prune(m, cp[p])
    if (is.numeric(dat[, p])){
      res <- get_res(nmiss = sum(i), residuals = residuals(m), type = add_residual)
      dat[i, p] <- predict(m, dat[i, , drop = FALSE]) + res
    } else if (is.logical(dat[, p])){
      dat[i, p] <- as.logical(predict(m, dat[i, , drop = FALSE]))
    } else { 
      dat[i, p] <- predict(m, dat[i, , drop = FALSE], type="class")
    }
  }
  dat
}


#' @rdname impute_tree
#' 
#' @export
impute_rf <- function(dat, formula, add_residual = c("none","observed","normal")
                      , na_action=na.omit, impute_all=FALSE, ...){
  if (not_installed("randomForest")) return(dat)
  stopifnot(inherits(formula,"formula"))
  add_residual <- match.arg(add_residual)
  do_by(dat, groups(dat,formula), .fun=rf_work, formula=remove_groups(formula)
    , add_residual=add_residual, na_action=na_action, impute_all=impute_all, ...)
}

rf_work <- function(dat, formula, add_residual, na_action, impute_all, ...){
  
  predictors <- get_predictors(formula, dat)
  predicted <- get_imputed(formula, dat)

  # Iterate over target variables to impute each of them.
  for (p in predicted) {
    # Get logical vector for rows to impute.
    i <- if (impute_all) rep(TRUE, nrow(dat)) else is.na(dat[, p])
    if (!any(i)) {
      next  # Skip if nothing to impute.
    }

    # Build model for p and use it to impute dat[i, p].
    # We need to work around a formula-handling bug in 'randomForest <= 4.6-12' 
    # (the ". - x" case is not handled correctly).
    # The following should work when randomForest gets updated.
    # p_formula <- as.formula(paste(p, " ~ ", deparse(formula[[3]])))
    p_predictors <- setdiff(predictors, p)
    p_formula <- as.formula(paste(p, " ~ ", paste(p_predictors, collapse=" + ")))
    m <- run_model(randomForest::randomForest, formula = p_formula, data = dat, na.action = na_action, ...)
    predicted_values <- predict(m, newdata = dat[i, , drop = FALSE])
    if (is.numeric(dat[, p]) && add_residual != "none") {
      cc <- complete.cases(dat[c(p, predictors)])
      cc_residuals <- dat[cc, p] - predict(m, newdata = dat[cc, , drop = FALSE])
      res <- get_res(nmiss = sum(i), residuals = cc_residuals, type = add_residual)
      dat[i, p] <- predicted_values + res
    } else {
      dat[i, p] <- predicted_values
    }
  }
  dat
}
