#'  simputation
#' 
#' @name simputation
#' @docType package
#' @import  stats
#' @importFrom MASS rlm 
#' @importFrom rpart rpart prune
#' @importFrom randomForest randomForest na.roughfix
#' @importFrom glmnet glmnet predict.glmnet
#' @importFrom gower gower_topn
#' @useDynLib simputation 
{}

#' @export
randomForest::na.roughfix