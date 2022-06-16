
not_installed <- function(pkg, action="Returning original data"){
  if(requireNamespace(pkg,quietly=TRUE)) return(FALSE)
  warnf("Package '%s' is needed but not found. %s", pkg, action)
  TRUE
}

#' Capabilities depending on suggested packages.
#'
#' This function has bevome unnecessary as of \code{simputation} 0.2.8 and higher.
#' It will be removed from future versions.
#' 
#' 
#'
#' @section details:
#'
#' \code{simputation_capabilities} Calls every \code{impute_} function and
#' grabs the warning message (if any) stating that a package is missing.
#'
#' \code{simputation_suggests} checks which of the suggested packages
#' implementing statistical models are available.
#'
#'
#' @return
#' For \code{simputation_capabilities} A named \code{character} vector of class
#' \code{simputation.capabilities}. The class attribute allows pretty-printing
#' of the output.
#'
#' @export
#' @keywords internal
simputation_capabilities <- function(){
  message("This function is unnecessary as of simputation 0.2.8 and will be removed in the next version.")
  funs <- getNamespaceExports("simputation")
  funs <- funs[grep("impute_",funs)]

  out <- rep("OK",length(funs))
  names(out) <- funs
  women <- data.frame(height=1:15, weight=1:15)

  L <- sapply(funs, function(fun){
    frm <- if (grepl("const",fun)) height ~ 1 else height ~ weight
    ifun <- getExportedValue("simputation",fun)
    tryCatch(ifun(women, frm), warning = function(e){
      out[fun] <<- gsub("\\..*", "", e$message)
    })
  })
  class(out) <- c("simputation.capabilities","character")
  out
}

#' @rdname simputation_capabilities
#' @param lib.loc Where to check whether a package is installed (passed to
#'   \code{\link[utils]{installed.packages}})
#'
#' @return
#' For \code{simputation_suggests} a \code{logical} vector, stating which
#' suggested packages are currently installed (\code{TRUE}) or not
#' (\code{FALSE}).
#'
#' @export
#' @keywords internal
simputation_suggests <- function(lib.loc=NULL){
  message("This function is unnecessary as of simputation 0.2.8 and will be removed in the next version.")
  # this function finds the actual dependencies, and faster
  # then utils::package_dependencies
  fl <- system.file("DESCRIPTION", package="simputation")
  if(file.exists(fl)){
    dcf <- read.dcf(fl)
  } else {
    cat(sprintf("Could not find DESCRIPTION file"))
  }
  sug <- trimws(strsplit(dcf[,"Suggests"], ",\\n?")[[1]])
  # remove pkgs not needed for model computation
  sug <- setdiff(sug,c("knitr","rmarkdown","dplyr","testthat"))
  inst <- rownames(installed.packages(lib.loc=lib.loc))
  out <- sug %in% inst
  names(out) <- sug
  out
}


#' print output of simputation_capabilities
#'
#' @param x an R object
#' @param ... unused
#'
#' @export
#' @keywords internal
print.simputation.capabilities <- function(x, ...){
  nm <- names(x)
  nm[1] <- paste0(" ",nm[1]," ")
  cat(sprintf("%-13s: %s\n",nm,x))
}


# Dummy model returned in case of estimation failure, to keep us going.
dummymodel <- function() structure(NA,class="dummymodel")
predict.dummymodel <- function(object,...) NA
residuals.dummymodel <- function(object,...) NA

run_model <- function(fun, ...){
    args <- list(...)
    tryCatch(do.call(fun,args), error = function(e){
    # Get predicted variable. list(...)[[1]] must be a formula object
    p <- all.vars(list(...)[[1]])[[1]]
    # get model name.
    a <- deparse(sys.call(-4L)[[2]])
    warnf("Could not execute %s for '%s': %s",a,p, e$message)
    dummymodel()
  })
}

# reasonable stopper/warner
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)
warnf <- function(fmt,...) warning(sprintf(fmt,...), call.=FALSE)

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

# General layout of imputation spec is
# [imputed vars] ~ [predictive vars] [| [grouping vars]]

has_groups <- function(frm){
  (length(frm) == 3L &&  length(frm[[3]]) == 3L && frm[[3]][[1]] == "|") ||
    (length(frm) == 2L && length(frm[[2]]) == 3L && frm[[2]][[1]] == "|" )
}

# get groups
groups <- function(dat, frm){
  grp <- character()
  # dplyr compatability:
  if (inherits(dat,"grouped_df")){
   grp <- sapply(attr(dat,"vars"), as.character)
  }
  # also take grouping from formula
  if (has_groups(frm)){
    n <- length(frm)
    grp <- c(grp,all.vars(frm[[n]][[3]]))
  }
  unique(grp)
}

# remove group statement from formula
remove_groups <- function(frm){
  n <- length(frm)
  if(has_groups(frm)) frm[[n]] <- frm[[n]][[2]]
  frm
}


do_by <- function(dat,groups,.fun,...){
  out <- if ( length(groups) == 0 ){
    .fun(dat, ...)
  } else {
    if(anyNA(dat[groups]))
      stopf("Cannot group data by %s: missing values detected.",paste(groups,collapse=","))
    # split-apply-combine the base-R way
    unsplit( lapply(split(as.data.frame(dat),dat[groups]),.fun,...), dat[groups])
  }
  # copy grouping or other attributes from the input
  attributes(out) <- attributes(dat)
  out
}



get_imputed <- function(frm, dat){
  if (length(frm)<3) return(character(0))
  if (!is_additive(frm[[2]])){
    stop(sprintf("Invalid specification of imputed variables: '%s'",deparse(frm[[2]]))
      ,call.=FALSE)
  }
  frm[[3]] <- frm[[2]]
  frm[[2]] <- 1
  x <- colnames(attr(terms(frm, data=dat),"factors"))
  # in case of back-ticked variables, use only the substring between
  # backticks (so backtics are not part of the string)
  if (!is.null(x)) gsub("`(.*?)`","\\1",x) else x
}




# Check expression against the following EBNF
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
  # normally 3, 2 for formulas of type ~ x
  n <- length(frm)
  if (one_ok) is_one <- frm[[n]] == 1

  if (  (one_ok && is_one) || is_additive(frm[[n]])  ){
    if( n == 3 ) frm[[2]] <- 1
    x <- colnames(attr(terms(frm, data=dat),"factors"))
    # in case of back-ticked variables, use only the substring between
    # backticks (so backtics are not part of the string)
    if (!is.null(x)) gsub("`(.*?)`","\\1",x) else x
  } else {
    stop(sprintf("Invalid specification of predictors %s:",deparse(frm[[n]])), call.=FALSE)
  }

}


#' @export
rpart::na.rpart

#' Rough imputation for handling missing predictors.
#'
#' This function is re-exported from
#' \code{\link[randomForest:na.roughfix]{randomForest:na.roughfix}} when
#' available. Otherwise it will throw a warning and resort to
#' \code{options("na.action")}
#'
#' @param object an R object caryying data (e.g. \code{data.frame})
#' @param ... arguments to be passed to other methods.
#'
#'
#'
#' @export
na.roughfix <- function(object,...){
  fn <- if (not_installed("randomForest","Resorting to options('na.action').")){
    options('na.action')
  } else {
    randomForest::na.roughfix
  }
  fn(object,...)
}

#' A \code{deparse} replacement that always returns a length-1 vector
#'
#' @param ... Arguments passed on to \code{base::deparse()}
#' @return The deparsed string
#'
#' @examples
#' long_formula <- this_is_a_formula_with_long_variables ~
#'   the_test_is_checking_if_deparse_will_return +
#'   multiple_strings_or_not
#' simputation:::deparse(long_formula)
deparse <- function(...) {
  orig_deparse <- base::deparse(...)
  paste(orig_deparse, collapse=" ")
}

#' Show the number of (remaining) missing values.
#' 
#' Quick indication of the amount and location of missing values.
#' The function uses \code{\link{na_status}} to print the missing values, but
#' returns the original \code{x} (invisibly) and therefore can be used in an imputation pipeline
#' to peek at the NA's status.
#' 
#' \code{glimpse_na} is especially helpful when interactively adding imputation methods.
#' \code{glimpse_na} is named after \code{\link[dplyr]{glimpse}} in \code{dplyr}.
#' 
#' Operator \code{\%?>\%} is syntactic sugar: it inserts a \code{glimpse_na} in
#' the pipe.
#'
#' @param x an R object caryying data (e.g. \code{data.frame})
#' @param show_only_missing if \code{TRUE} only columns with \code{NA}'s will be
#' printed.
#' @param ... arguments passed to \code{\link{na_status}}.
#' @examples
#' 
#' 
#'    
#' irisNA <- iris
#' irisNA[1:3,1] <- irisNA[3:7,2] <- NA
#' 
#' # How many NA's?
#' na_status(irisNA)
#' 
#' # add an imputation method one at a time
#' iris_imputed <-
#'   irisNA |> 
#'   glimpse_na() # same as above
#' 
#' # ok, glimpse_na says "Sepal.Width" has NA's
#' # fix that:
#' 
#' iris_imputed <-
#'   irisNA |> 
#'   impute_const(Sepal.Width ~ 7) |> 
#'   glimpse_na() # end NA
#' 
#' # Sepal.Length is having NA's
#' 
#' iris_imputed <-
#'   irisNA |> 
#'   impute_const(Sepal.Width ~ 7) |>
#'   impute_cart(Sepal.Length ~ .) |>  
#'   glimpse_na() # end NA
#' 
#' # in an existing imputation pipeline we can peek with
#' # glimpse_na or %?>%
#' 
#' iris_imputed <-
#'   irisNA |> 
#'   glimpse_na() |>     # shows the begin NA
#'   impute_const(Sepal.Width ~ 7) |> 
#'   glimpse_na() |>     # after 1 imputation
#'   impute_cart(Sepal.Length ~ .) |>  
#'   glimpse_na()         # end NA
#'   
#' # or
#' iris_imputed <-
#'   irisNA %?>% 
#'   impute_const(Sepal.Width ~ 7) %?>% 
#'   impute_cart(Sepal.Length ~ .)
#'   
#' na_status(iris_imputed)  
#' 
#' @export
glimpse_na <- function(x, show_only_missing = TRUE, ...){
  if (is.data.frame(x)){
    nas <- na_status(x, show_only_missing = TRUE, ...)
    if (nrow(nas)){
      print(nas)
    }
  }
  invisible(x)
}

#' Show the number of (remaining) missing values.
#' 
#' Quick indication of the amount and location of missing values.
#' @examples 
#' irisNA <- iris
#' irisNA[1:3,1] <- irisNA[3:7,2] <- NA
#' na_status(irisNA)
#'
#' # impute a constant 
#' a <- impute_const(irisNA, Sepal.Width ~ 7)
#' na_status(a)
#' @return \code{data.frame} with the column and number of NA's
#' @seealso \code{\link{glimpse_na}}
#' @export
#' @param x an R object caryying data (e.g. \code{data.frame})
#' @param show_only_missing if \code{TRUE} only columns with \code{NA}'s will be
#' printed.
#' @param sort_columns If \code{TRUE} the columns are sorted descending 
#' by the number of missing values.
#' @param show_message if \code{TRUE} message will be printed.
#' @param ... arguments to be passed to other methods.
na_status <- function( x
                     , show_only_missing = TRUE
                     , sort_columns = show_only_missing
                     , show_message = TRUE
                     , ...
                     ){
  if (!is.data.frame(x)){
    stop("na_status works only only data.frame's", call. = FALSE)
  }
  col_nas <- sapply(x, function(v){
    sum(is.na(v))
  })
    
  if (isTRUE(show_only_missing)){
    col_nas <- col_nas[col_nas > 0]
  }
  # sort from high to low
  if (isTRUE(sort_columns)){
    col_nas <- rev(sort(col_nas))
  }
  d <- data.frame( columns = names(col_nas)
                 , nNA     = unname(col_nas)
  )
  if (nrow(d) > 0){
    if (show_message) {
      message("\nna count: ", sum(col_nas),"")
    }
    d
  } else {
    if (show_message) {message("\nNo NA's.")}
    invisible(d)
  }
}

#' @export 
#' @rdname glimpse_na
#' @param lhs left hand side of pipe
#' @param rhs right hand side of pipe
`%?>%` <- function(lhs, rhs){
  # using dplyr instead of magrittr otherwise this 
  # generates a check-as-cran warning
  if (requireNamespace("dplyr", quietly = TRUE)){
    `%>%` <- dplyr::`%>%`
    e <- substitute(lhs %>% glimpse_na() %>% rhs)
    eval(e)
  } else {
    stop("This method depends on package `dplyr`.", call. = FALSE)
  }
}

