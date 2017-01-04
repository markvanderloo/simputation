#'  simputation
#' 
#' @name simputation
#' @docType package
#' @import  stats
#' @importFrom MASS rlm 
#' @importFrom rpart rpart prune
#' @importFrom gower gower_topn
#' @useDynLib simputation 
{}

#' Rough imputation for handling missing predictors.
#' 
#' This function is re-exported from 
#' \code{\link[randomForest:na.roughfix]{randomForest:na.roughfix}} when 
#' available. Otherwise it will throw a warning and resort to 
#' \code{options("na.action")}
#' 
#' @param object an R object caryying data (e.g. \code{data.frame})
#' @param ... arguments to be passed to other methods.
#' 
#' 
#' 
#' @export
na.roughfix <- function(object,...){
  fn <- if (not_installed("randomForest","Resorting to options('na.action').")){
    options('na.action')
  } else {
    randomForest::na.roughfix
  }
  fn(object,...)
}
