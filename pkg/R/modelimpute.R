
#' Impute using a previously fitted model.
#'
#' Impute one or more variables using a single R object representing a 
#' previously fitted model.
#' 
#' @section Model specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_OBJECT }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. The right-hand-side must be a model object for which an \code{S3}
#' \code{predict} method is implemented. Alternatively, one can specify a custom
#' predicting function. This function must accept at least a model and a
#' dataset, and return one predicted value for each row in the dataset.
#' 
#' \code{\link{foretell}} implements usefull \code{predict} methods for cases
#' where by default the predicted output is not of the same type as the predicted
#' variable (e.g. when using certain link functions in \code{glm})
#'
#' 
#'    
#' @param dat \code{[data.frame]} The data to be imputed.
#' @param formula \code{[formula]} object of the form 
#'    \code{<imputed variables> ~ <model object>}
#' @param predictor \code{[function]} with signature \code{object, newdata, ...}
#'   that returns predicted values given a model \code{object} and a new dataset
#'   \code{newdata}. By default \code{\link{foretell}} is used.
#' @param ... Extra arguments passed to \code{predictor}
#' 
#' @family imputation
#' 
#' @export
#' @examples 
#' 
#' irisNA <- iris
#' iris[1:3,1] <- NA
#' my_model <- lm(Sepal.Length ~ Sepal.Width + Species, data=iris)
#' impute(irisNA, Sepal.Length ~ my_model)
#'
impute <- function(dat, formula, predictor = foretell, ...){
  model <- eval(formula[[3]])
  imputed <- get_imputed(formula, dat)
  
  args <- list(object=model, newdata=dat,...)
  
  pred_val <- tryCatch(do.call(predictor,args), error=function(e){
    warnf("Could not compute predictions:\n%s\nReturning original data.", e$message)
    NULL
  })
  if (is.null(pred_val)) return(dat)
  if (length(pred_val) != nrow(dat))
    warnf("Numberof values returned by the predictor is not equal to number of rows in data")
  
  for (var in imputed){
    ina <- is.na(dat[var])
    dat[ina,var] <- pred_val[ina]
  }
  dat
}


#' @section Details:
#' 
#' \code{impute_} is an explicit version of \code{impute} that works better in
#' programming contexts, especially in cases involving nonstandard evaluation.
#'
#' @rdname impute
#' @param variables \code{[character]} Names of columns in \code{dat} to impute.
#' @param model A model object.
impute_ <- function(dat, variables, model, predictor=foretell,...){
  imputed <- variables
  args <- list(object=model, newdata=dat,...)
  
  pred_val <- tryCatch(do.call(predictor,args), error=function(e){
    warnf("Could not compute predictions:\n%s\nReturning original data.", e$message)
    NULL
  })
  if (is.null(pred_val)) return(dat)
  if (length(pred_val) != nrow(dat))
    warnf("Numberof values returned by the predictor is not equal to number of rows in data")
  
  for (var in imputed){
    ina <- is.na(dat[var])
    dat[ina,var] <- pred_val[ina]
  }
  dat
}

