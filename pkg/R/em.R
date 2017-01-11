


#' @rdname impute_
#' @param p2s verbosity of \code{\link[Amelia]{amelia}}: \code{0}: no output,
#'  \code{1} print iterations to screen.
#'
#' @export
impute_em <- function(dat, formula, p2s=0, ...){
  if ( not_installed("Amelia") ) return(dat)
  prd <- get_predictors(formula, dat)
  imp <- get_imputed(formula, dat)
  imp <- unique(c(imp, prd))
  
  dat[imp] <- tryCatch(Amelia::amelia(dat[imp], m=1, p2s=p2s, ...)$imputations[[1]]
       , error = function(e){
         warnf("Amelia::amelia stopped with message\n %s\n Returning original data"
               , e$message)
         dat[imp]
       })  
  dat
}
