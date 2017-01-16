

#' @rdname impute_
#' 
#' @export
impute_em <- function(dat, formula, p2s=0,...){
  if ( not_installed("Amelia") ) return(dat)
  grp <- groups(dat,formula)
  frm <- remove_groups(formula)
  prd <- get_predictors(frm, dat)
  imp <- get_imputed(formula, dat)
  imp <- unique(c(imp, prd))
  
  imp_work <- function(dd){
    d <- dd[imp]
    # We need this ugly escape since Amelia sends errors when passed a
    # complete dataset.
    if (!anyNA(d)) return(dd)
    # Ok, let's get to work
    out <- tryCatch({Amelia::amelia(d, m=1, p2s=p2s, boot.type="none", ...)}
      , error = function(e){
        warnf("Amelia::amelia stopped with message\n %s\n Returning original data"
              , e$message)
        FALSE
    }) # end tryCatch
    # if Amelia stopped, return data untouched
    if (identical(out,FALSE)) return(dd)
    
    # extract parameters (computed on z-transformed columns)
    mu_sc <- out$mu
    cov_sc <- out$covMatrices[,,1]
    
    # z-transform columns for imputation
    x <- d
    mu_x <- colMeans(x,na.rm=TRUE)
    sd_x <- apply(x,2,sd,na.rm=TRUE)
    x_sc <- scale(x,center=mu_x, scale=sd_x)
    
    # Impute scaled values. For derivation of imputation equation see e.g.
    # http://fourier.eng.hmc.edu/e161/lectures/gaussianprocess/node7.html
    a <- apply(x_sc,1,function(x_){
      i_miss <- is.na(x_)
      if (!any(i_miss)) return(x_)
      x_obs <- x_[!i_miss]
      mu_miss <- mu_sc[i_miss]
      mu_obs <- mu_sc[!i_miss]
      Smo <- cov_sc[i_miss,!i_miss,drop=FALSE]
      Soo <- cov_sc[!i_miss,!i_miss,drop=FALSE]
      x_[i_miss] <- mu_miss + Smo%*%solve(Soo,(x_obs - mu_obs))
      x_
    })
    dd[imp] <- as.data.frame(unscale(t(a),mu=mu_x,sd=sd_x))
    dd
  }
  
  do_by(dat=dat, groups=grp, .fun = imp_work)  

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
  grp <- groups(dat, formula)
  frm <- remove_groups(formula)
  prd <- get_predictors(frm, dat)
  imp <- get_imputed(formula, dat)
  imp <- unique(c(imp, prd))

  imp_work <- function(dd){
    d <- dd[imp]
    # workaround since amelia errors when no missings present
    if (!anyNA(d)) return(dd)
    # actual work
    out <- tryCatch(Amelia::amelia(d, m=1, p2s=p2s, ...)$imputations[[1]]
      , error = function(e){
       warnf("Amelia::amelia stopped with message\n %s\n Returning original data"
          , e$message
        ) # end tryCatch
      FALSE
    })
    if (identical(out,FALSE)) return(dd)
    dd[imp] <- out
    dd
  }
  
  do_by(dat, grp, imp_work)

}
