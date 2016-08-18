
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




get_imputed <- function(frm, dat){
  if (!is_additive(frm[[2]])){
    stop(sprintf("Invalid specification of imputed variables: '%s'",deparse(frm[[2]]))
      ,call.=FALSE)
  }
  frm[[3]] <- frm[[2]]
  frm[[2]] <- 1
  colnames(attr(terms(frm, data=dat),"factors"))
}




# Chec expression against the following EBNF
#
# NAME      = <A valid R symbol name>
# UNARYMIN  = "-"
# BINOP     = "+" | "-"
# PREDICTED = [UNARYMIN] NAME [BINOP PREDICTED] 
#
is_additive <- function(expr,val=TRUE){
  if (length(expr)==1) return(is.symbol(expr)) 
  
  if (deparse(expr[[1]]) %in% c("+", "-") ){
    for (i in seq_along(expr)[-1]) val <- val & is_additive(expr[[i]],val)
  } else {
    val <- FALSE
  }
  val
}



# frm: a formula
# dat: data.frame
# one_ok: is <lhs> ~ 1 also ok?
get_predictors <- function(frm, dat, one_ok = FALSE){
  if (one_ok) is_one <- frm[[3]] == 1
  
  if (  (one_ok && is_one) || is_additive(frm[[3]])  ){
    frm[[2]] <- 1
    colnames(attr(terms(frm, data=dat),"factors"))
  } else {
    stop(sprintf("Invalid specification of predictors %s:",deparse(frm[[3]])), call.=FALSE)    
  }
  # get rid of variables on lhs so predictors using "." get listed correctly.

}


