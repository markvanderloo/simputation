
# ------------------------------------------------------------------------------
# RANDOM HOTDECK IMPUTATION

#' Hot deck imputation
#' 
#' Hot-deck imputation methods include random and sequential hot deck, 
#' k-nearest neighbours imputation and predictive mean matching.
#' 
#' 
#' @param dat \code{[data.frame]}, with variables to be imputed and their
#'   predictors.
#' @param formula \code{[formula]} imputation model description (see Details below).
#' @param backend \code{[character]} Choose the backend for imputation. If 
#' \code{backend="VIM"} the variables used to sort the data (in case of
#'  sequential hot deck) may not coincide with imputed variables.
#' @param pool \code{[character]} Specify donor pool when \code{backend="simputation"}
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
#' @param prob \code{[numeric]} Sampling probability weights (passed through to
#'   \code{\link[base]{sample}}). Must be of length \code{nrow(dat)}.
#' @param ... further arguments passed to \code{\link[VIM:hotdeck]{VIM::hotdeck}}
#'   if \code{VIM} is chosen as backend, otherwise they are passed to
#' \itemize{
#'   \item{\code{\link[base]{order}} for \code{impute_shd}} and
#'   \code{backend="simputation"} 
#'   \item{\code{\link[VIM:hotdeck]{VIM::hotdeck}}
#'   for \code{impute_shd} and \code{impute_rhd} when \code{backend="VIM"}}.
#'   \item{\code{\link[VIM:kNN]{VIM:kNN}} for \code{impute_knn} when 
#'   \code{backend="VIM"}}
#'   \item{The \code{predictor} function for \code{impute_pmm}.}
#' }
#' 
#' 
#' @section Model specification:
#' 
#' Formulas are of the form
#' 
#' \code{IMPUTED_VARIABLES ~ MODEL_SPECIFICATION [ | GROUPING_VARIABLES ] }
#' 
#' The left-hand-side of the formula object lists the variable or variables to 
#' be imputed. The interpretation of the independent variables on the
#' right-hand-side depends on the imputation method.
#' 
#' \itemize{
#' \item{\code{impute_rhd} Variables in \code{MODEL_SPECIFICATION} and/or 
#' \code{GROUPING_VARIABLES} are used to split the data set into groups prior to
#' imputation. Use \code{~ 1} to specify that no grouping is to be applied.}
#' \item{\code{impute_shd} Variables in \code{MODEL_SPECIFICATION} are used to 
#' sort the data. When multiple variables are specified, each variable after
#' the first serves as tie-breaker for the previous one.}
#' \item{\code{impute_knn} The predictors are used to determine Gower's distance
#' between records (see \code{\link[gower]{gower_topn}}). This may include the
#' variables to be imputed.}.
#' \item{\code{impute_pmm}} Predictive mean matching. The
#'  \code{MODEL_SPECIFICATION} is passed through to the \code{predictor}
#'  function.
#'} 
#' 
#' 
#' If grouping variables are specified, the data set is split according to the
#' values of those variables, and model estimation and imputation occur
#' independently for each group.
#' 
#' Grouping using \code{dplyr::group_by} is also supported. If groups are 
#' defined in both the formula and using \code{dplyr::group_by}, the data is 
#' grouped by the union of grouping variables. Any missing value in one of the 
#' grouping variables results in an error.
#' 
#' @section Methodology:
#' 
#' \bold{Random hot deck imputation} with \code{impute_rhd} can be applied to
#' numeric, categorical or mixed data. A missing value is copied from a sampled
#' record. Optionally samples are taken within a group, or with non-uniform
#' sampling probabilities. See Andridge and Little (2010) for an overview
#' of hot deck imputation methods.
#' 
#' \bold{Sequential hot deck imputation} with \code{impute_rhd} can be applied
#' to numeric, categorical, or mixed data. The dataset is sorted using the
#' `predictor variables'. Missing values or combinations thereof are copied
#' from the previous record where the value(s) are available in the case
#' of LOCF and from the next record in the case of NOCF. 
#'   
#' \bold{Predictive mean matching} with \code{impute_pmm} can be applied to
#' numeric data. Missing values or combinations thereof are first imputed using
#' a predictive model. Next, these predictions are replaced with observed
#' (combinations of) values nearest to the prediction. The nearest value is the
#' observed value with the smallest absolute deviation from the prediction.
#' 
#' \bold{K-nearest neighbour imputation} with \code{impute_knn} can be applied 
#' to numeric, categorical, or mixed data. For each record containing missing 
#' values, the \eqn{k} most similar completed records are determined based on
#' Gower's (1977) similarity coefficient. From these records the actual donor is
#' sampled.
#' 
#' 
#' @section Using the VIM backend:
#'
#' The \href{ https://CRAN.R-project.org/package=VIM}{VIM} package has efficient
#' implementations of several popular imputation methods. In particular, its 
#' random and sequential hotdeck implementation is faster and more
#' memory-efficient than that of the current package. Moreover, \pkg{VIM} offers
#' more fine-grained control over the imputation process then \pkg{simputation}.
#' 
#' If you have this package installed, it can be used by setting
#' \code{backend="VIM"} for functions supporting this option. Alternatively, one
#' can set \code{options(simputation.hdbackend="VIM")} so it becomes the
#' default. 
#' 
#' 
#' Simputation will map the simputation call to a function in the
#' \pkg{VIM} package. In particular:
#' 
#'  \itemize{
#'  \item{\code{impute_rhd} is mapped to \code{VIM::hotdeck} where imputed
#'  variables are passed to the \code{variable} argument and the union of
#'  predictor and grouping variables are passed to \code{domain_var}.
#'  Extra arguments in \code{...} are passed to \code{VIM::hotdeck} as well.
#'  Argument \code{pool} is ignored.}
#'  \item{\code{impute_shd} is mapped to \code{VIM::hotdeck} where
#'  imputed variables are passed to the \code{variable} argument, predictor
#'  variables to \code{ord_var} and grouping variables to \code{domain_var}.
#'  Extra arguments in \code{...} are passed to \code{VIM::hotdeck} as well.
#'  Arguments \code{pool} and \code{order} are ignored. In \code{VIM} the donor pool
#'  is determined on a per-variable basis, equivalent to setting \code{pool="univariate"}
#'  with the simputation backend. \pkg{VIM} is LOCF-based. Differences between
#'  \pkg{simputation} and \code{VIM} likely occurr when the sorting variables contain missings.}
#'  \item{\code{impute_knn} is mapped to \code{VIM::kNN} where imputed variables
#'  are passed to \code{variable}, predictor variables are passed to \code{dist_var}
#'  and grouping variables are ignored with a message. 
#'  Extra arguments in \code{...} are passed to \code{VIM::kNN} as well.
#'  Argument \code{pool} is ignored.
#'  Note that simputation  adheres stricktly to the Gower's original
#'  definition of the distance measure, while \pkg{VIM} uses a generalized variant
#'  that can take ordered factors into account.
#'  }
#' }
#' By default, \pkg{VIM}'s imputation functions add indicator variables to the
#' original data to trace what values have been imputed. This is switched off by
#' default for consistency with the rest of the simputation package, but it may
#' be turned on again by setting \code{imp_var=TRUE}.
#' 
#' 
#' @references 
#' Andridge, R.R. and Little, R.J., 2010. A review of hot deck imputation for
#' survey non-response. International statistical review, 78(1), pp.40-64.
#' 
#' Gower, J.C., 1971. A general coefficient of similarity and some of its
#' properties. Biometrics, pp.857--871.
#' 
#' @name impute_hotdeck
#' @rdname impute_hotdeck
#' @family imputation
#' @export
impute_rhd <- function(dat, formula, pool=c("complete","univariate","multivariate")
                       , prob, backend=getOption("simputation.hdbackend",default=c("simputation","VIM"))
                       , ...){
  stopifnot(inherits(formula,"formula"))
  backend <- match.arg(backend, choices = c("simputation","VIM"))
  at <- attributes(dat)
  predicted <- get_imputed(formula, dat)
  grps <- groups(dat,formula)
  formula <- remove_groups(formula)
  predictors <- get_predictors(formula, dat, one_ok = TRUE)
  predictors <- unique(c(predictors, grps))

  if ( backend == "VIM" ){ 
    return(hd_vim(data=dat
          , variable=predicted
          , ord_var=NULL # no ordering in random hotdeck
          , domain_var=predictors
          , ...))
  }
  
  
  pool <- match.arg(pool)
  
  rhd <- switch(pool
      , complete     = cc_rhd
      , univariate   = single_rhd
      , multivariate = multi_rhd)
  
  

  
  idat <- dat[predicted]
  # ugly construction, but fast.
  prob <- if (missing(prob)) rep(1,nrow(dat)) else {stopifnot(length(prob)==nrow(dat)); prob}
  idat$PROB..TMP <- prob

  spl <- if (length(predictors) > 0) dat[predictors] else data.frame(split=rep(1,nrow(dat)))
  
  # split-apply-combine, the base-R way.
  dat[predicted] <- unsplit( lapply( split(idat, spl), rhd ), spl)[predicted]
  attributes(dat) <- at
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

## Map hotdeck methods to VIM backend
hd_vim <- function(data, variable, ord_var, domain_var, imp_var=FALSE,...){
  if(not_installed("VIM")) return(data)
  if (length(domain_var) == 0) domain_var <- NULL
  tryCatch(
    VIM::hotdeck(data=data, variable=variable
      , ord_var=ord_var,domain_var=domain_var
      ,imp_var=imp_var,...)
    , error = function(e){
      warnf("VIM::hotdeck stopped with message\n %s\n Returning original data."
            , e$message)
      data
    })
}


# ------------------------------------------------------------------------------
# SEQUENTIAL HOTDECK IMPUTATION


#' @rdname impute_hotdeck
#' @param order \code{[character]} Last Observation Carried Forward or Next
#'   Observarion Carried Backward. Only for \code{backend="simputation"}
#' 
#' @export
impute_shd <- function(dat, formula, pool=c("complete","univariate","multivariate")
                       , order=c("locf","nocb")
                       , backend=getOption("simputation.hdbackend", default=c("simputation","VIM"))
                       , ...){
  stopifnot(inherits(formula,"formula"))
  backend <- match.arg(backend,choices=c("simputation","VIM"))
  if (backend == "VIM"){
    predictors <- get_predictors(formula,dat,one_ok = TRUE)
    if (length(predictors)==0) predictors <- ".vim.doemmy.sortvar"
    dat$.vim.doemmy.sortvar <- seq_len(nrow(dat))
    out <- hd_vim(data=dat
      , variable = setdiff(get_imputed(formula, dat), predictors)
      , ord_var = predictors
      , domain_var = groups(dat, formula)
      , ...
    )
    out$.vim.doemmy.sortvar <- NULL
    return(out)
  }
  
  # else: use builtin backend
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
  
  ord <- if( length(predictors) > 0){
    do.call("order", dat[predictors]) # decreasing=FALSE by default 
  } else {
    seq_len(nrow(dat)) 
  }
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


#' @rdname impute_hotdeck
#' @param predictor \code{[function]} Imputation to use for predictive part in
#'   predictive mean matching. Any \code{impute_} function of this package that
#'   supports the \code{impute_all} argument can be used.
#' 
#' @export
impute_pmm <- function(dat, formula, predictor=impute_lm
  , pool=c('complete','univariate','multivariate'), ...){
  
  # input check
  stopifnot(inherits(formula,"formula"))
  is_valid_predictor <- "impute_all" %in% names(formals(predictor))
  if (!is_valid_predictor) {
    error_fmt <- paste(
      "Cannot use '%s' as predictor for predictive mean matching."
      , "Predictor function must support the 'impute_all' argument."
    )
    stopf(error_fmt, deparse(substitute(predictor)))
  }
  pool <- match.arg(pool)

  # generate predictions by imputing with the 'predictor' function.
  do_by(dat, groups(dat,formula), .fun=pmm_work
    , predictor = predictor
    , formula=remove_groups(formula), pool=pool, ...)
}

pmm_work <- function(dat, formula, predictor=impute_lm
                       , pool=c('complete','univariate','multivariate'), ...){
  
  # generate predictions by imputing with the 'predictor' function.
  idat <- predictor(dat = dat, formula = formula, impute_all = TRUE, ...)
  predicted <- get_imputed(formula, dat)

  # call appropriate workhorse imputation function
  switch(pool
     , complete = multi_cc_pmm(dat,idat,predicted, TRUE)
     , univariate = single_pmm(dat,idat,predicted)
     , multivariate = multi_cc_pmm(dat,idat,predicted,FALSE)
  )
}

# dat: original data
# idat: formula-imputed data
# predicted [character]: variables imputed in idat
single_pmm <- function(dat, idat, predicted) {
  for (p in predicted) {
    is_na_dat <- is.na(dat[, p])
    is_na_idat <- is.na(idat[, p])
    # Get logical vectors with donor and recipient rows.
    # Note that idat[, p] can have missings in records where dat[, p] does not.
    # Such records should not be selected as donor. This can be handled when
    # calculating the distance between imputed recipient and potential donor,
    # but it is more robust to exclude these records from the donor pool here.
    is_donor_row <- !is_na_dat & !is_na_idat
    is_recipient_row <- is_na_dat & !is_na_idat
    if (!any(is_donor_row) || !any(is_recipient_row)) {
      next  # No donors or nothing to impute.
    }
    # For each recipient, get index of closest donor using gower_topn().
    # Note that gower_topn() cannot handle the variable p having 0 range.
    closest_donor_idx <- if (calc_range(idat[, p]) == 0) {
      # Impute the first donor value. This is consistent with the behaviour of
      # gower_topn(), which returns the lowest index of equal values (for n=1).
      1
    } else {
      gower::gower_topn(idat[is_recipient_row, p, drop = FALSE]
                        , idat[is_donor_row, p, drop = FALSE]
                        , n = 1L)$index
    }
    # Translate donor indices to indices in the full data set.
    j <- which(is_donor_row)[closest_donor_idx]
    # Impute donor values into recipients.
    dat[is_recipient_row, p] <- dat[j, p]
  } 
  dat
}

# dat: original data
# idat: formula-imputed data
# predicted [character]: variables imputed in idat
# only_complete [logical]: TRUE: complete cases only, FALSE: by missingness pattern
multi_cc_pmm <- function(dat, idat, predicted, only_complete = TRUE) {
  is_na_dat <- is.na(dat)
  is_na_idat <- is.na(idat)
  # Get positions of missings in dat that have been imputed in idat.
  # These are the positions where we want to impute values with pmm.
  # Note that idat only differs from dat in predicted variables, meaning that
  # all values of M outside of predicated variables are FALSE.
  M <- is_na_dat & !is_na_idat
  # get missing data patterns 
  mdp <- unique(M)
  # Transpose of M is used to find rows that match a missing data pattern.
  M <- t(M)
  # Get positions of potential donor values.
  is_donor_val <- !is_na_dat & !is_na_idat
  if (only_complete) {
    # Donors are rows of the data that are complete for all predicted vars.
    is_donor_row <- rowSums(is_donor_val[, predicted, drop = FALSE]) == length(predicted)
  }
  for (i in seq_len(nrow(mdp))) {
    # get missing data pattern for imputed variables
    pat <- mdp[i,]
    if (!any(pat)) {
      next  # No missings in pattern, nothing to impute.
    }
    if (!only_complete) {
      # Donors are rows of the data that are complete for pat.
      is_donor_row <- rowSums(is_donor_val[, pat, drop = FALSE]) == sum(pat)
    }
    if (!any(is_donor_row)) {
      next  # Cannot impute without donors.
    }
    # Recipient rows are those rows of the data that match pat.
    is_recipient_row <- colSums(M == pat) == length(pat)
    # For each recipient, get index of closest donor using gower_topn().
    # Note that gower_topn() cannot handle all variables in pat having 0 range.
    is_range_0 <- (sapply(idat[, pat, drop = FALSE], calc_range) == 0)
    closest_donor_idx <- if (all(is_range_0)) {
      # Impute the first donor value. This is consistent with the behaviour of
      # gower_topn(), which returns the lowest index of equal values (for n=1).
      1
    } else {
      gower::gower_topn(idat[is_recipient_row, pat, drop = FALSE]
                        , idat[is_donor_row, pat, drop = FALSE]
                        , n = 1L)$index
    }
    # Translate donor indices to indices in the full data set.
    j <- which(is_donor_row)[closest_donor_idx]
    # Impute donor values into recipients.
    dat[is_recipient_row, pat] <- dat[j, pat]
  }
  dat
}

# Calculate the range of a vector v, i.e., the difference between max and min.
# Note that v is assumed to have at least one non-NA value.
calc_range <- function(v) {
  range <- max(v, na.rm = TRUE) - min(v, na.rm = TRUE)
  if (!is.finite(range)) {
    stop("Failed to calculate range.")
  }
  range
}


# ------------------------------------------------------------------------------
# KNN IMPUTATION



#' @rdname impute_hotdeck
#' @param k \code{[numeric]} Number of nearest neighbours to draw the donor from.
#' 
#' @export
impute_knn <- function(dat, formula
  , pool=c("complete","univariate","multivariate")
  , k=5
  , backend = getOption("simputation.hdbackend", default=c("simputation","VIM"))
  , ...){
  stopifnot(inherits(formula,"formula"))
  backend <- match.arg(backend,choices=c("simputation","VIM"))
  
  if (backend == "VIM"){
    if (length(groups(dat, formula)) > 0){
      message("VIM does not support grouping. Ignoring grouping variables")
    }
    return(knn_vim(data=dat
      , variable = get_imputed(formula, dat)
      , dist_var = get_predictors(formula, dat)
      , k=k , ... ))
  }
  
  pool <- match.arg(pool)
  
  do_by(dat, groups(dat,formula), .fun=knn_work
    , formula=remove_groups(formula), pool=pool, k=k, ...)
}

## knn imputation with VIM backend.
knn_vim <- function(data, variable, dist_var, imp_var=FALSE, k=5,...){
  if (not_installed("VIM")) return(data)
  tryCatch(
    VIM::kNN(data=data, variable=variable, dist_var=dist_var
     , imp_var=imp_var, k=k,...)
    , error = function(e){
      warnf("VIM::kNN stopped with message\n %s\n Returning original data."
            , e$message)
      data
    })
  
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



