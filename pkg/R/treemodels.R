

#' @rdname impute_
#' @export
impute_cart <- function(dat, model, add_residual=c("none","observed","normal"),...){
  add_residual <- match.arg(add_residual)
  worknl(dat=dat, model=model, add_residual=add_residual,...)
}

worknl <- function(dat, model, add_residual,...){
  predictors <- get_predictors(model, names(dat))
  predicted <- get_predicted(model, names(dat))
  formulas <- paste(predicted, "~" ,deparse(model[[3]]) )

  for (i in seq_along(predicted)){
    p <- predicted[i]
    ina <- is.na(dat[,p])
    m <- rpart::rpart(formula = as.formula(formulas[i]),dat=dat, ...)
    if (is.numeric(dat[,p])){
      res <- get_res(nmiss = sum(ina),residuals = residuals(m), type=add_residual)
      dat[ina,p] <- predict(m,dat[ina,,drop=FALSE]) + res
    } else if (is.logical(dat[,p])){
      dat[ina,p] <- as.logical(predict(m, dat[ina,,drop=FALSE]))
    } else { 
      dat[ina,p] <- predict(m, dat[ina,,drop=FALSE],type="class")
    }
  }
  dat
}






