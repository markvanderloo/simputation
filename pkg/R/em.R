

#' @rdname impute_
#' 
#' @export
impute_em <- function(dat, formula, p2s=0,...){
  if ( not_installed("Amelia") ) return(dat)
  prd <- get_predictors(formula, dat)
  imp <- get_imputed(formula, dat)
  imp <- unique(c(imp, prd))
  
  out <- tryCatch(Amelia::amelia(dat[imp], m=1, p2s=p2s, boot.type="none", ...)
       , error = function(e){
         warnf("Amelia::amelia stopped with message\n %s\n Returning original data"
               , e$message)
         dat
  })  
  if (identical(out,dat)) return(dat)

  mu_sc <- out$mu
  cov_sc <- out$covMatrices[,,1]
  
  # scale for imputation
  x <- dat[imp]
  mu_x <- colMeans(x,na.rm=TRUE)
  sd_x <- apply(x,2,sd,na.rm=TRUE)
  x_sc <- scale(x,center=mu_x, scale=sd_x)
  
  # impute scaled values. For derivation of imputation equation 
  # See http://fourier.eng.hmc.edu/e161/lectures/gaussianprocess/node7.html
  a <- apply(x_sc,1,function(x_){
    i_miss <- is.na(x_)
    if (!any(i_miss)) return(x_)
    x_obs <- x_[!i_miss]
    mu_miss <- mu_sc[i_miss]
    mu_obs <- mu_sc[!i_miss]
    Som <- cov_sc[i_miss,!i_miss,drop=FALSE]
    Soo <- cov_sc[!i_miss,!i_miss,drop=FALSE]
    x_[i_miss] <- mu_miss + Som%*%solve(Soo,(x_obs - mu_obs))
    x_
  })
  # unscale and insert into original data
  dat[imp] <- unscale(t(a),mu=mu_x,sd=sd_x)
  dat
}

unscale <- function(x,mu,sd){
  for ( i in seq_len(ncol(x)) ){
    x[,i] <- sd[i]*x[,i] + mu[i]
  }
  x
}




#' @rdname impute_
#' @param p2s verbosity of \code{\link[Amelia]{amelia}}: \code{0}: no output,
#'  \code{1} print iterations to screen.
#'
#' @export
impute_emb <- function(dat, formula, p2s=0, ...){
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
