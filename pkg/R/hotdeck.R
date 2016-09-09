
# ------------------------------------------------------------------------------
# RANDOM HOTDECK IMPUTATION


#' @section Hot deck imputation:
#' 
#' 
#' \itemize{
#' \item{\code{impute_rhd} The predictor variables in the \code{model} argument are used to split the data 
#' set into groups prior to imputation (use \code{~ 1} to specify that no grouping is applied).}
#' \item{\code{impute_shd} The predictor variables are used to sort the data.}
#' \item{\code{impute_knn} The predictors are used to determine Gower's distance 
#'  between records (see \code{\link[gower]{gower_topn}})}.
#'} 
#' 
#' The \code{pool} argument is used to specify the donor pool as follows.
#' \itemize{
#' \item{\code{"complete"}. Only records for which the variables on the
#'    left-hand-side of the model formula are complete are used as donors. If a
#'    record has multiple missings, all imputations are taken from a single 
#'    donor.}
#' \item{\code{"univariate"}. Imputed variables are treated one by one and
#'    independently so the order of variable imputation is unimportant. If a 
#'    record has multiple missings, separate donors are drawn for each missing 
#'    value.}
#' \item{\code{"multivariate"}. A donor pool is created for each missing data 
#'    pattern. If a record has multiple missings, all imputations are taken from 
#'    a single donor.}
#' } 
#' 
#' 
#'
#'
#' @rdname impute_
#' @param pool Specify donor pool. See under 'Hot deck imputation'.
#' @param prob \code{[numeric]} Sampling probability weights (passed through to
#'   \code{\link[base]{sample}}). Must be of length \code{nrow(dat)}.
#' @export
impute_rhd <- function(dat, formula, pool=c("complete","univariate","multivariate"), prob, ...){
  stopifnot(inherits(formula,"formula"))
  pool <- match.arg(pool)
  
  rhd <- switch(pool
      , complete     = cc_rhd
      , univariate   = single_rhd
      , multivariate = multi_rhd)
  
  
  prob <- if (missing(prob)) rep(1,nrow(dat)) else {stopifnot(length(prob)!=nrow(dat)); prob}

  predicted <- get_imputed(formula, dat)
  grps <- groups(dat,formula)
  formula <- remove_groups(formula)
  predictors <- get_predictors(formula, dat, one_ok = TRUE)
  predictors <- unique(c(predictors, grps))
  
  idat <- dat[predicted]
  # ugly construction, but fast.
  idat$PROB..TMP <- prob
    
  spl <- if (length(predictors) > 0) dat[predictors] else data.frame(split=rep(1,nrow(dat)))
  
  # split-apply-combine, the base-R way.
  dat[predicted] <- unsplit( lapply( split(idat, spl), rhd ), spl)[predicted]

  dat
}


# random hot deck, comlete cases pool
cc_rhd <- function(x){
  ic <- complete.cases(x)
  if ( !any(ic) || all(ic) ) return(x) # no donors or no missings
  prb <- x$PROB..TMP[ic]
  ina <- is.na(x)
  nna <- sum(ina)
  jdn <- isample(which(ic),size=nna, replace=TRUE, prob=prb) 
  
  A <- array(c(jdn,which(ina,arr.ind=TRUE)[,2]), dim=c(nna,2))
  x[ina] <- x[A]
  x
}

# random hot deck, column by column
single_rhd <- function(x){
  for ( i in seq_along(x)[-length(x)]){
    ina <- is.na(x[,i])
    if ( !any(ina) || all(ina) ) next
    x[ina,i] <- isample(x[!ina,i], size=sum(ina), replace=TRUE,prob=x$PROB..TMP[!ina])
  }
  x
}

# random hot deck, donors per missing data pattern.
multi_rhd <- function(x){
  nvar <- length(x)
  M <- is.na(x)[,-nvar,drop=FALSE]
  
  if (sum(M)==0) return(x) # nothing to do
   
  # derive missing data patterns (one pattern per row)
  mdp <- unique(M)

  # donor pool for each missing data pattern (columns represent donors)
  donor_pool <- iapply(mdp,1,function(pat) complete.cases(x[pat]))
  
  # recipient set for each missing data pattern
  M <- t(M)
  p <- nvar - 1 # number of actual variables (last is probability vector)
  recipient_set <- iapply(mdp,1,function(pat){
    colSums(M==pat) == p
  })
  
  for (i in seq_len(nrow(mdp))) {
    pat <- mdp[i,]
    if ( sum(pat) == 0 || sum(donor_pool[,i])==0 ) next # nothing missing, or no donors
    donors <- x[donor_pool[,i],pat,drop=FALSE]
    j <- isample(seq_len(nrow(donors)), size=sum(recipient_set[,i]), replace=TRUE, prob = x$PROB..TMP )
    x[recipient_set[,i],pat] <- donors[j,,drop=FALSE]
  }
  x
}

# ------------------------------------------------------------------------------
# SEQUENTIAL HOTDECK IMPUTATION


#' @rdname impute_
#' @param order Last Observation Carried Forward or Next Observarion Carried Backward
#' @export
impute_shd <- function(dat, formula, pool=c("complete","univariate","multivariate")
                       , order=c("locf","nocb"),...){
  stopifnot(inherits(formula,"formula"))
  pool <- match.arg(pool)
  order <- match.arg(order)
  
  do_by(dat, groups(dat,formula), .fun=shd_work
    , formula=remove_groups(formula), pool=pool, order=order, ...)
}

shd_work <- function(dat, formula, pool=c("complete","univariate","multivariate")
                       , order=c("locf","nocb"),...){
  stopifnot(inherits(formula,"formula"))
  predicted <- get_imputed(formula, dat)
  predictors <- get_predictors(formula, dat, one_ok=TRUE)
  
  
  ord <- if( length(predictors) > 0) order(dat[predictors],decreasing=FALSE) else seq_len(nrow(dat)) 
  if (order=="locf") ord <- rev(ord)
  
  # create index to reverse ordering later.
  ind <- order(ord)
 
  # order imputed variables.
  idat <- dat[ord, predicted, drop=FALSE]
  
  if (pool == "complete"){
    idat <- cc_shd(idat)
  } else if (pool == "univariate"){
    for ( p in predicted ) idat[,p] <- single_shd(idat[,p])
  } else if( pool == "multivariate") {
    idat <- multi_shd(idat)
  }
  # reorder
  dat[predicted] <- idat[ind,,drop=FALSE]
  dat
}

# determine indices of donors for recipients
donor_indices <- function(x, is_present, is_missing){
  npresent <- sum(is_present)
  subindices <- ( npresent - rev(cumsum(rev(is_present))) ) %% npresent + 1
  subindices[is_missing]
}


  
# sequential NOCB hotdeck, for each column
single_shd <- function(x){
  is_donor <- !is.na(x)
  if (any(is_donor)){
    # donor_indices() recycles from the top if the last element is missing.
    # we prevent this by first imputing the LOCF for that position.
    if (!is_donor[length(x)]){
      x[length(x)] <- x[max(which(is_donor))]
      is_donor[length(x)] <- TRUE
    }
    donors <- x[is_donor]
    is_missing <- is.na(x)
    replace(x, is_missing, donors[donor_indices(x,is_donor,is_missing)])
  } else {
    x
  }
}


# sequential NOCB hotdeck, with complete cases as donor
cc_shd <- function(x){
  donor_pool <- complete.cases(x)
  if ( all(donor_pool)||!any(donor_pool) ) return(x) # nothing to do
  
  recipient_set <- !donor_pool
  #donor_pool <- which(donor_pool)
 
  j <- donor_indices(x,donor_pool, recipient_set)
  donors <- x[which(donor_pool)[j],,drop=FALSE]
  ina <- is.na(x[recipient_set,,drop=FALSE])
  x[recipient_set,][ina] <- donors[ina]
  x
}

# sequential NOCB hotdeck, per missing data pattern
multi_shd <- function(x){
  M <- is.na(x)
  
  # derive missing data patterns
  mdp <- unique(M)
  if ( nrow(mdp) == 0 ) return(x) # nothing to do..
  
  # donor pool for each missing data pattern (each column is one donor pool)
  donor_pool <- iapply(mdp,1,function(pat) complete.cases(x[pat]))
  
  # recipient set for each missing data pattern
  M <- t(M)
  nvar <- length(x)
  recipient_set <- iapply(mdp,1,function(pat){
    colSums(M==pat) == nvar
  })
  
  for (i in seq_len(nrow(mdp))) {
    pat <- mdp[i,]
    if (sum(pat) == 0 || sum(donor_pool[,i]) == 0) next # nothing missing, or no donors
    donors <- x[donor_pool[,i],pat,drop=FALSE]
    j <- donor_indices(x,donor_pool[,i],recipient_set[,i])
    x[recipient_set[,i],pat] <- donors[j,,drop=FALSE]
  }
  x
}

# ------------------------------------------------------------------------------
# PMM IMPUTATION


#' @rdname impute_
#' @param predictor \code{[function]} Imputation to use for predictive part in
#'   predictive mean matching. Any of the \code{impute_} functions of this
#'   package (it makes no sense to use a hot-deck imputation).
#' @export
impute_pmm <- function(dat, formula, predictor=impute_lm
  , pool=c('complete','univariate','multivariate'), ...){

  # input check
  stopifnot(inherits(formula,"formula"))
  pool <- match.arg(pool)
  
  # generate predictions by imputing with the 'predictor' function.
  do_by(dat, groups(dat,formula), .fun=pmm_work
    , formula=remove_groups(formula), pool=pool, ...)
}

pmm_work <- function(dat, formula, predictor=impute_lm
                       , pool=c('complete','univariate','multivariate'), ...){
  
  # generate predictions by imputing with the 'predictor' function.
  idat <- predictor(dat=dat,formula=formula,...)
  predicted <- get_imputed(formula, dat)
  predicted <- names(dat) %in% predicted
  # call appropriate workhorse imputation function
  switch(pool
     , complete = multi_cc_pmm(dat,idat,predicted, TRUE)
     , univariate = single_pmm(dat,idat,predicted)
     , multivariate = multi_cc_pmm(dat,idat,predicted,FALSE)
  )
  
}




# dat: original data
# idat: formula-imputed data
# predicted [logical] which variables have been imputed
single_pmm <- function(dat, idat, predicted){
  for ( p in which(predicted) ){
    don <- dat[!is.na(dat[,p]),p]
    iimp <- is.na(dat[,p]) & !is.na(idat[,p])
    if ( length(don)==0 || sum(iimp)==0) next # no donors, or nothing to impute
    idat[iimp,p] <- .Call("pmm_impute_dbl",as.double(idat[iimp,p]),as.double(don))
  } 
  idat
}

# dat: original data
# idat: formula-imputed data
# predicted [logical] which variables have been imputed
# only_complete:[logical] TRUE: complete cases only, FALSE: by missingness pattern
# (only_complete=FALSE)
multi_cc_pmm <- function(dat, idat, predicted, only_complete=TRUE){
  M <- is.na(dat) & !is.na(idat)
  # get missing data patterns 
  mdp <- unique(M)
  M <- t(M)
  imputed <- logical(nrow(dat))
  if (only_complete) donor_pool <- complete.cases(dat[predicted])
  for ( i in seq_len(nrow(mdp))){
    # get missing data pattern for imputed variables
    pat <- mdp[i,]
    # skip if that leaves us with nothing
    if (!any(pat)) next
    # only donors that have not been imputed earlier
    if(!only_complete) donor_pool <- complete.cases(dat[pat]) & !imputed
    # no donors or nothing to impute: skip.
    if (!any(donor_pool)||all(donor_pool)) next 
    # recycle over columns of M to find recipients with pattern 'pat'
    recipients <- colSums(M==pat) == length(pat)
    # get closest match (scaled L1 distance)
    topn <- gower::gower_topn(idat[recipients,pat,drop=FALSE]
                  , dat[donor_pool,pat,drop=FALSE], n=1L)
    # index from donor pool to actual dataset
    j <- which(donor_pool)[topn$index]
    # impute the bastard; update imputation administration
    dat[recipients,pat] <- dat[j,pat]
    imputed <- imputed | recipients
  }
  dat
}




# ------------------------------------------------------------------------------
# KNN IMPUTATION



#' @rdname impute_
#' @param k Number of nearest neighbours to draw the donor from.
#' @export
impute_knn <- function(dat, formula, pool=c("complete","univariate","multivariate"), k=5, ...){
  stopifnot(inherits(formula,"formula"))
  pool <- match.arg(pool)
  
  do_by(dat, groups(dat,formula), .fun=knn_work
    , formula=remove_groups(formula), pool=pool, k=k, ...)
}


knn_work <- function(dat, formula, pool=c("complete","univariate","multivariate"), k, ...){
  
  predicted <- get_imputed(formula, dat)
  predictors <- get_predictors(formula, dat)
  
  # choose imputation function.
  knn <- switch(pool
      , complete = cc_knn
      , univariate = single_knn
      , multivariate = multi_knn
  )
 
  knn(dat, imp_vars = predicted, match_vars = predictors, k=k)
}



## knn-imputation, complete cases as donor pool
cc_knn <- function(dat, imp_vars, match_vars, k){
  ic <- complete.cases(dat[imp_vars])
  if (all(ic)||!any(ic)) return(dat) # nothing to do..
  dat[!ic,] <- do_knn(
    recipients = dat[!ic,,drop=FALSE]
    , donor_pool = dat[ic,,drop=FALSE]
    , imp_vars=imp_vars, match_vars = match_vars, k=k)
  dat
}

## per-column knn-imputation
single_knn <- function(dat, imp_vars, match_vars, k){
  # temporary var so by-variable imputations are independent.
  idat <- dat 
   for ( p in imp_vars ){
     ina <- is.na(dat[,p])
     dat[ina,] <- do_knn(recipients = idat[ina,,drop=FALSE], donor_pool = idat[!ina,,drop=FALSE]
        , imp_vars=p, match_vars = match_vars, k=k)
   }
  dat
}

## Per-missing data pattern knn-imputation
multi_knn <-  function(dat, imp_vars, match_vars, k){
  dimp <- dat[imp_vars]
  M <- is.na(dimp)
  if (sum(M)==0) return(dat) # nothing to impute
  
  # unique missing data patterns (one per row)
  mdp <- unique(M)
  # all missing data patterns, (one per column)
  M <- t(M)
  
  # loop over missing data patterns
  for ( i in seq_len(nrow(mdp)) ){
    pat <- mdp[i,]
    if ( !any(pat) ) next # nothing to do.
    
    # find records with missing pattern 'pat' (recycling over columns of M)
    recip <- which(colSums(M==pat)==length(pat))
    
    # find records where variables according to 'pat' are complete.
    ic <- complete.cases(dimp[pat])
    
    # skip if no donors available
    if (!any(ic) ) next 
    
    # impute
    dat[recip,] <- do_knn(recipients = dat[recip,,drop=FALSE]
        , donor_pool = dat[ic,,drop=FALSE]
        , imp_vars = imp_vars, match_vars=match_vars, k=k)
  }
  dat
}


# generic knn imputation. 
do_knn <- function(recipients, donor_pool, imp_vars, match_vars, k){
  # check if we have enough donors to draw from. Otherwise lower k.
  ndon <- nrow(donor_pool)
  if ( k > ndon ){
    warning(sprintf("Requested k = %d while %d donors present. Using k = %d."
                    , k, ndon, ndon),call.=FALSE)
    k <- nrow(donor_pool)
  }
  
  # compute top-k matches.
  L <- gower::gower_topn(recipients[match_vars], donor_pool[match_vars], n=k)

  # draw indices in the table of closest matches.
  j <- isample(seq_len(k), size=nrow(recipients), replace=TRUE)
  
  # get donors from index-table of closest matches
  A <- matrix(c(j,seq_along(j)),nrow=length(j)) 
  donors <- donor_pool[L$index[A],imp_vars,drop=FALSE]

  # impute and return completed dataset.
  ina <- is.na(recipients[imp_vars])
  recipients[imp_vars][ina] <- donors[ina]

  recipients
}



