

#' @rdname impute_
impute_cart <- function(data, model, add_residual=c("none","observed","normal"),...){
  add_residual <- match.arg(add_residual)
  worknl(data=data, model=model, add_residual=add_residual,...)
}

worknl <- function(data, model, add_residual,...){
  predictors <- get_predictors(model, names(data))
  predicted <- get_predicted(model, names(data))
  formulas <- paste(predicted, "~" ,deparse(model[[3]]) )

  for (i in seq_along(predicted)){
    p <- predicted[i]
    ina <- is.na(data[,p])
    m <- rpart::rpart(formula = as.formula(formulas[i]),data=data, ...)
    if (is.numeric(data[,p])){
      res <- get_res(nmiss = sum(ina),residuals = residuals(m), type=add_residual)
      data[ina,p] <- predict(m,data[ina,,drop=FALSE]) + res
    } else if (is.logical(data[,p])){
      data[ina,p] <- as.logical(predict(m, data[ina,,drop=FALSE]))
    } else { 
      data[ina,p] <- predict(m, data[ina,,drop=FALSE],type="class")
    }
  }
  data
}
