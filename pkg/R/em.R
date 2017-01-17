

#' Multivariate, model-based imputation
#' 
#' Models that simultaneously optimize imptuation of multiple variables.
#' Methods include imputation based on EM-estimation of multivariate normal
#' parameters, imputation based on iterative Random Forest estimates and
#' stochastic imptuation based on bootstrapped EM-estimatin of multivariate
#' normal parameters.
#' 
#' @rdname impute_multivariate
#' @name impute_multivariate
#' 
#' @param dat \code{[data.frame]} with variables to be imputed.
#' @param formula \code{[formula]} imputation model description 
#' @param verbose \code{[numeric]} Control amount of output printed to screen.
#' Higher values mean more output. 
#' \itemize{
#' \item{0 or a number \eqn{\geq 1} for \code{impute_em}}
#' \item{0, 1, or 2 for \code{impute_emb}}
#' }
#'    
#' @param ... Options passed to 
#' \itemize{
#' \item{\code{\link[norm:em.norm]{norm::em.norm}} for \code{impute_em} }
#' \item{\code{\link[Amelia:amelia]{Amelia::amelia}} for \code{impute_emb}}
#' \item{\code{\link[missForest:missForest]{missForest::missForest}} for \code{impute_mf}}
#' }
#'
#' @section Model specification:
#'   
#' 
#' @export
impute_em <- function(dat, formula, verbose=0,...){
  if ( not_installed("norm") ) return(dat)
  grp <- groups(dat,formula)
  frm <- remove_groups(formula)
  prd <- get_predictors(frm, dat)
  imp <- get_imputed(formula, dat)
  imp <- unique(c(imp, prd))
  
  imp_work <- function(dd){
    d <- as.matrix(dd[imp])
    if (!anyNA(d)) return(dd)
    # Ok, let's get to work
    s <- norm::prelim.norm(d)
    theta <- tryCatch(norm::em.norm(s,showits=(verbose>0),...)
      , error = function(e){
        warnf("norm::em.norm stopped with message\n %s\n Returning original data")
        FALSE
    })
    # if em.norm stopped, return data untouched
    if (identical(theta,FALSE)) return(dd)
    
    # extract parameters (computed on z-transformed columns)
    # see code of norm::getparam.norm for extraction
    mu_sc <- theta[s$psi[1, 2:(s$p + 1)]] 
    cov_sc <- theta[s$psi[2:(s$p + 1), 2:(s$p + 1)]]
    cov_sc <- matrix(cov_sc, s$p, s$p)
    
    # z-transform columns for imputation
    x <- d
    mu_x <- colMeans(x,na.rm=TRUE)
    sd_x <- apply(x,2,sd,na.rm=TRUE)
    x_sc <- scale(x,center=mu_x, scale=sd_x)
    
    # Impute scaled values. For derivation of imputation equation see e.g.
    # http://fourier.eng.hmc.edu/e161/lectures/gaussianprocess/node7.html
    a <- apply(x_sc,1,function(x_){
      i_miss <- is.na(x_)
      i_obs <- !i_miss
      if (!any(i_miss)) return(x_)
      x_obs <- x_[i_obs]
      mu_miss <- mu_sc[i_miss]
      mu_obs <- mu_sc[i_obs]
      Smo <- cov_sc[i_miss, i_obs, drop=FALSE]
      Soo <- cov_sc[i_obs , i_obs, drop=FALSE]
      x_[i_miss] <- mu_miss + Smo%*%solve(Soo,(x_obs - mu_obs))
      x_
    })
    dd[imp] <- unscale(t(a),mu=mu_x,sd=sd_x)
    dd
  }
  
  do_by(dat=dat, groups=grp, .fun = imp_work)  

}

# inverse z-transform, x: data.frame or matrix
unscale <- function(x,mu,sd){
  for ( i in seq_len(ncol(x)) ){
    x[,i] <- sd[i]*x[,i] + mu[i]
  }
  x
}




#' @rdname impute_multivariate
#'
#' @export
impute_emb <- function(dat, formula, verbose=0, ...){
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
    out <- tryCatch(Amelia::amelia(d, m=1, p2s=verbose, ...)$imputations[[1]]
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

#' @rdname impute_multivariate
#' 
#' @export
impute_mf <- function(dat, formula,...){
  stopifnot(inherits(formula,"formula"))
  if ( not_installed("missForest") ) return(dat)
  imputed <- get_imputed(formula,dat)
  predictors <- get_predictors(formula, dat,...)
  vars <- unique(c(imputed,predictors))
  imp <- tryCatch(missForest::missForest(dat[vars])[[1]], error=function(e){
    warnf("Could not execute missForest::missForest: %s\n Returning original data"
         , e$message)
    dat
  })
  dat[imputed] <- imp[imputed]
  dat
}
