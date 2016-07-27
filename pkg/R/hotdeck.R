

#' @rdname impute_
#' @param donor_set Determine donors set for each variable or once for all imputed variables.
#' @param prob \code{[numeric]} Sampling probability weights (passed through to \code{\link[base]{sample}}). 
#'         Must be of length \code{nrow(dat)}.
#' @export
impute_rhd <- function(dat, model, donor_set=c("per-variable","common-donor"), prob, ...){
  
  stopifnot(inherits(model,"formula"))
  donor <- match.arg(donor_set)
  
  rhd <- if (donor == "per-variable") single_rhd else multi_rhd
  
  if (missing(prob)) prob <- rep(1,nrow(dat)) else stopifnot(length(prob)!=nrow(dat))
  dat[length(dat) + 1] <- prob
  
  predicted <- get_predicted(model,names(dat))
  predictors <- get_predictors(model,names(dat))
  
  spl <- if (length(predictors) > 0) dat[predictors] else data.frame(split=rep(1,nrow(dat)))
  
  # split-apply-combine, the base-R way.
  dat[predicted] <- unsplit( lapply( split(dat, spl), rhd ), spl)[predicted]

  # remove column with probability weights
  dat[-length(dat)]
}

# random hot deck, column by column
single_rhd <- function(x){
  for ( i in seq_along(x)){
    ina <- is.na(x[,i])
    if ( !any(ina) || all(ina) ) next
    x[ina,i] <- sample(x[!ina,i], size=sum(ina), replace=TRUE,prob=x[!ina,length(x)])
  }
  x
}

# random hot-deck, for each column
multi_rhd <- function(x){
  ic <- complete.cases(x)
  # find donor for everybody (allows for easy indexing in loop)
  idon <- sample(which(ic), size=nrow(x), replace=TRUE, prob=x[!ina, length(x)])
  for ( i in seq_along(x)){
    ina <- is.na(x[,i])
    if ( !any(ina) || all(ina) ) next
    x[ina,i] <- x[idon[ina],i]
  }
  x
}


#' @rdname impute_
#' @param order Last Observation Carried Forward or Next Observarion Carried Backward
#' @param pool Create a donor pool for each variable (\code{"single"}) or create a donor
#' pool for each missingess pattern (\code{"multiple"}).
#' @export
impute_shd <- function(dat, model, order=c("locf","nocb"), pool=c("single","multiple"),...){
  stopifnot(inherits(model,"formula"))
  predicted <- get_predicted(model,names(dat),no_pp_overlap=FALSE)
  predictors <- get_predictors(model,names(dat))
  
  pool <- match.arg(pool)
  
  order <- match.arg(order)
  ord <- if( length(predictors) > 0) order(dat[predictors],decreasing=FALSE) else seq_len(nrow(dat)) 
  if (order=="locf") ord <- rev(ord)
  
  # create index to reverse ordering later.
  ind <- order(ord)
 
  # order imputed variables.
  idat <- dat[ord, predicted, drop=FALSE] 
  if (pool == "single"){
    for ( p in predicted ) idat[,p] <- single_shd(idat[,p])
  } else {
    idat <- multi_shd(idat)
  }
  # reorder
  dat[predicted] <- idat[ind,,drop=FALSE]
  dat
}


# determine indices of donors voor recipients
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


multi_shd <- function(x){
  M <- is.na(x)
  
  # derive missing data patterns
  mdp <- unique(M)[-1,,drop=FALSE]
  if ( nrow(mdp) == 0 ) return(x) # nothing to do..
  
  # donor pool for each missing data pattern
  donor_pool <- apply(mdp,1,function(pat) complete.cases(x[pat]))
  
  # recipient set for eachmissing data pattern
  M <- t(M)
  nvar <- length(x)
  recipient_set <- apply(mdp,1,function(pat){
    colSums(M==pat) == nvar
  })
  
  for (i in seq_len(nrow(mdp))) {
    pat <- mdp[i,]
    donors <- x[donor_pool[,i],pat,drop=FALSE]
    j <- donor_indices(x,donor_pool[,i],recipient_set[,i])
    x[recipient_set[,i],pat] <- donors[j,,drop=FALSE]
  }
  x
}






