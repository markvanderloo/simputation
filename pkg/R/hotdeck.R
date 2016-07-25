


#' @rdname impute_
#' @export
impute_rhd <- function(dat, model, donor_sample=c("per-variable","per-record"), ...){
  
  stopifnot(inherits(model,"formula"))
  donor <- match.arg(donor_sample)
  
  rhd <- if (donor == "per-variable") single_rhd else multi_rhd
  
  predicted <- get_predicted(model,names(dat))
  predictors <- get_predictors(model,names(dat))
  
  spl <- if (length(predictors) > 0) dat[predictors] else data.frame(split=rep(1,nrow(dat)))
  
  # split-apply-combine, the base-R way.
  dat[predicted] <- unsplit( lapply( split(dat, spl), rhd ), spl)[predicted]
  dat
}

# random hot deck, column by column
single_rhd <- function(x){
  for ( i in seq_along(x)){
    ina <- is.na(x[,i])
    if ( !any(ina) || all(ina) ) next
    x[ina,i] <- sample(x[!ina,i], size=sum(ina), replace=TRUE)
  }
  x
}

# random hot-deck, for each column
multi_rhd <- function(x){
  ic <- complete.cases(x)
  # find donor for everybody (allows for easy indexing in loop)
  idon <- sample(which(ic), size=nrow(x), replace=TRUE)
  for ( i in seq_along(x)){
    ina <- is.na(x[,i])
    if ( !any(ina) || all(ina) ) next
    x[ina,i] <- x[idon[ina],i]
  }
  x
}




