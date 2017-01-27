

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
#' Higher values mean more output, typically per iteration.
#' \itemize{
#' \item{0 or a number \eqn{\geq 1} for \code{impute_em}}
#' \item{0, 1, or 2 for \code{impute_emb}}
#' }
#'    
#' @param ... Options passed to 
#' \itemize{
#' \item{\code{\link[norm:em.norm]{norm::em.norm}} for \code{impute_em} }
#' \item{\code{\link[missForest:missForest]{missForest::missForest}} for \code{impute_mf}}
#' }
#'
#' @section Model specification:
#'   
#' 
#' Formulas are of the form
#' 
#' \code{[IMPUTED_VARIABLES] ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' When \code{IMPUTED_VARIABLES} is empty, every variable in 
#' \code{MODEL_SPECIFICATION} will be imputed. When \code{IMPUTED_VARIABLES} is 
#' specified, all variables in \code{IMPUTED_VARIABLES} and 
#' \code{MODEL_SPECIFICATION} are part of the model, but only the 
#' \code{IMPUTED_VARIABLES} are imputed in the output.
#' 
#' \code{GROUPING_VARIABLES} specify what categorical variables are used to
#' split-impute-combine the data. Grouping using \code{dplyr::group_by} is also
#' supported. If groups are defined in both the formula and using
#' \code{dplyr::group_by}, the data is grouped by the union of grouping
#' variables. Any missing value in one of the grouping variables results in an
#' error.
#' 
#' @section Methodology:
#' 
#' \bold{EM-based imputation} with \code{impute_em} only works for numerical
#' variables. These variables are assumed to follow a multivariate normal distribution
#' for which the means and covariance matrix is estimated based on the EM-algorithm
#' of Dempster Laird and Rubin (1977). The imputations are the expected values
#' for missing values, conditional on the value of the estimated parameters.
#' 
#' 
#' \bold{Multivariate Random Forest imputation} with \code{impute_mf} works for
#' numerical, categorical or mixed data types. It is based on the algorithm
#' of Stekhoven and Buehlman (2012). Missing values are imputed using a
#' rough guess after which a predictive random forest is trained and used
#' to re-impute themissing values. This is iterated until convergence.
#' 
#' 
#' @references 
#' Dempster, Arthur P., Nan M. Laird, and Donald B. Rubin. "Maximum likelihood
#' from incomplete data via the EM algorithm." Journal of the royal statistical
#' society. Series B (methodological) (1977): 1-38.
#' 
#' Stekhoven, D.J. and Buehlmann, P., 2012. MissForest---non-parametric missing 
#' value imputation for mixed-type data. Bioinformatics, 28(1), pp.112-118.
#' 
#' @export
impute_em <- function(dat, formula, verbose=0,...){
  if ( not_installed("norm") ) return(dat)
  grp <- groups(dat,formula)
  frm <- remove_groups(formula)
  prd <- get_predictors(frm, dat)
  im1 <- get_imputed(formula, dat)
  imp <- unique(c(im1, prd))
  
  if ( !all(sapply(dat[imp],is.numeric)) ){
    warnf("Cannot impute non-numeric variables. Returning original data")
    return(dat)
  }
  
  imp_work <- function(dd){
    d <- as.matrix(dd[imp])
    if (!anyNA(d)) return(dd)
    # Ok, let's get to work
    s <- tryCatch(norm::prelim.norm(d)
      , error = function(e){
       warnf("norm::prelim.norm stopped with message\n %s\n Returning original data"
             , e$message)
        FALSE
      })
    if (identical(s,FALSE)) return(dd)
    theta <- tryCatch(norm::em.norm(s,showits=(verbose>0),...)
      , error = function(e){
        warnf("norm::em.norm stopped with message\n %s\n Returning original data"
              ,e$message)
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
  
  a <- do_by(dat=dat, groups=grp, .fun = imp_work)  
  if (length(im1) == 0){
    a
  } else {
    dat[im1] <- a[im1]
    dat
  }
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
impute_mf <- function(dat, formula,...){
  stopifnot(inherits(formula,"formula"))
  if ( not_installed("missForest") ) return(dat)
  imputed <- get_imputed(formula,dat)
  predictors <- get_predictors(formula, dat,...)
  vars <- unique(c(imputed,predictors))
  imp <- tryCatch(missForest::missForest(dat[vars],...)[[1]], error=function(e){
    warnf("Could not execute missForest::missForest: %s\n Returning original data"
         , e$message)
    dat
  })
  if ( length(imputed) == 0 ){
    dat[vars] <- imp[vars]
  } else {
    dat[imputed] <- imp[imputed]
  }
  dat
}
