
# Give 'sample' reasonable behaviour
isample <- function(x, size, replace=FALSE, prob=NULL){
  if (length(x)==1) rep(x,size) else sample(x,size,replace,prob)
}

# give 'apply' reasonable behaviour.
iapply <- function(X, MARGIN, FUN, ...){
  a <- apply(X, MARGIN, FUN, ...)
  if (!is.array(a)) dim(a) <- c(length(a),1)
  a
}


# frm: a formula
# vars: variable names
get_predictors <- function(frm, vars){
  v <- all.vars(frm[[3]])
  w <-v[!v %in% vars]
  if( length(w) > 0)
    stop(sprintf("Predictors %s not found in data",paste(w,collapse=", ")))
  v
}


# frm: a formula
# vars: variable names
# no_pp_overlap: check for overlap between predictors/predicted
get_predicted <- function(frm, vars, no_pp_overlap=TRUE){
  v <- all.vars(frm[[2]])
  w <- all.vars(frm[[3]])
  if ( identical(v , ".") ){ 
    v <- vars
    v <- setdiff(v,w)
  }
  if (no_pp_overlap && any(v %in% w))
    stop(sprintf("Using '%s' as predictor and predicted"
                  , paste(v,collapse=", ")), call.=FALSE)
  w <- v[!v %in% vars]
  if (length(w)>0)
    stop(sprintf("Trying to impute variables not in data: %s"
                 ,paste(w,collapse=", ")), call.=FALSE)
  # TODO: allow '.' and minus signs
  v
}

