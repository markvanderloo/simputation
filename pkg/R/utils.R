
# Dummy model returned in case of estimation failure, to keep us going.
dummymodel <- function() structure(NULL,class="dummymodel")
predict.dummymodel <- function(object,...) NA
residuals.dummymodel <- function(object,...) NA

run_model <- function(fun, ...){
    tryCatch(fun(...), error = function(e){
    # Get predicted variable. list(...)[[1]] must be a formula object
    p <- all.vars(list(...)[[1]])[[1]]
    # get model name.
    #b <- as.character(sys.call(-4L))
    a <- deparse(sys.call(-4L)[[2]])
    warning(sprintf("Could not execute %s for '%s': %s",a,p, e$message)
            , call.=FALSE)
    dummymodel()
  })
}



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
  w <-v[!v %in% c(vars,".")]
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

