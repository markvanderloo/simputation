
context("Formula parsing")
test_that("formula parsing",{
  expect_true(is_additive(expression(x)[[1]]))
  expect_true(is_additive(expression(x + y)[[1]]))
  expect_true(is_additive(expression(x - z)[[1]]))
  expect_true(is_additive(expression(.-x)[[1]]))
  expect_false(is_additive(expression(x*y)[[1]]))

  expect_equal(get_imputed(x + y ~ z, dat=data.frame(x=0,y=0,z=0)  ),c("x","y"))
  expect_equal(get_imputed(.-y ~ x + y, dat=data.frame(x=0,y=0,z=0)),c("x","z"))
  expect_equal(get_imputed(. ~ x + y, dat=data.frame(x=0,y=0,z=0)),c("x","y","z"))
  expect_error(get_imputed(x:y+z ~ x, dat=data.frame(x=0,y=0,z=0)))

  expect_equal(get_predictors(z ~ x + y, dat=data.frame(x=0,y=0,z=0)),c("x","y"))
  expect_equal(get_predictors(z ~ .-y, dat=data.frame(x=0,y=0,z=0)), c("x","z"))
  expect_equal(get_predictors(z ~ . , dat=data.frame(x=0,y=0,z=0)), c("x","y","z"))
  
  expect_null(get_predictors(x + y ~ 1, dat=data.frame(x=0,y=0,z=0), one_ok=TRUE))

  expect_error(get_predictors(x ~ x:y+z, dat=data.frame(x=0,y=0,z=0)))
  
})


context("Linear model imputation")
test_that("stuff gets imputed",{
  funs <- list(impute_lm = impute_lm
               , impute_rlm = impute_rlm
               , impute_const = impute_const
               , impute_median = impute_median)
  irisNA <- iris
  irisNA[1:3,"Sepal.Length"] <- NA
  irisNA[3:5,"Sepal.Width"] <- NA
  irisNA[5:7,"Petal.Length"] <- NA
  for ( i in seq_along(funs)){
    f <- funs[[i]]
    fun <- names(funs)[i]
    # impute one variable, constant models
    expect_equal(sum(is.na(f(irisNA,Sepal.Length ~ 1))),  6,info=fun)
    # impute two variables, constant models
    expect_equal(sum(is.na(f(irisNA,Sepal.Length + Sepal.Width ~ 1))),  3,info=fun)
    # impute all variables, constant models. 
    expect_equal(sum(is.na(f(irisNA, . ~ 1))),  0,info=fun)
    # with residuals 
    expect_equal(sum(is.na(f(irisNA, . ~ 1, add_residual="normal"))),  0,info=fun)
    expect_equal(sum(is.na(f(irisNA, . ~ 1, add_residual="observed"))),  0,info=fun)
  }

  funs <- list(impute_lm = impute_lm
               , impute_rlm = impute_rlm
               , impute_cart = impute_cart
               , impute_proxy = impute_proxy)
  for ( i in seq_along(funs) ){
    fn <- funs[[i]]
    nm <- names(funs)[i]
    out <- if (nm == "impute_cart") 6 else 7 # cart is more robust for missing predictors
    expect_equal(sum(is.na(fn(irisNA, Sepal.Length ~ Sepal.Width))), out, info=nm)  
    expect_equal(sum(is.na(fn(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width))),3,info=nm)
    expect_equal(sum(is.na(fn(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width, add_residual="normal"))),3,info=nm)
    expect_equal(sum(is.na(fn(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width, add_residual="observed"))),3,info=nm)
    
  }
  if (requireNamespace("glmnet",quietly = TRUE)){
    # Imputation using glmnet. Expects two predictors at minimum (intercept not counted)
    # add dummy variable
    nm <- "impute_en"
    irisNA$PW2 <- 2 * irisNA$Petal.Width
    expect_equal(sum(is.na(impute_en(irisNA, Sepal.Length ~ Sepal.Width + PW2))), 7, info=nm)  
    expect_equal(sum(is.na(impute_en(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width + PW2))),3,info=nm)
    expect_equal(sum(is.na(impute_en(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width + PW2, add_residual="normal"))),3,info=nm)
    expect_equal(sum(is.na(impute_en(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width + PW2, add_residual="observed"))),3,info=nm)
  }
  
  # Try imputing a column where everything is missing (so models don't fit)
  irisNA$foo <- NA_real_
  for ( f in list(impute_lm, impute_rlm, impute_median) ){
    expect_warning(f(irisNA, foo ~ 1))
  }
})





test_that("grouped imputation",{
  wom <- women
  wom$foo <- c(rep("a",7),rep("b",8))
  wom[c(1,15),1] <- NA

  expect_equal(impute_lm(wom, height ~ 0 + foo), impute_lm(wom, height ~ 1 | foo))
  expect_equal(impute_rlm(wom, height ~ 0 + foo), impute_lm(wom, height ~ 1 | foo))
  expect_equal(impute_median(wom, height ~ foo),impute_median(wom, height ~ 1|foo))
  expect_equal(impute_proxy(wom, height ~ weight), impute_proxy(wom, height ~ weight | foo))
  expect_equal(impute_const(wom, height ~ 7), impute_const(wom, height ~ 7 | foo))
  wom2 <- wom1 <- wom
  wom1[c(1,15),1] <- mean(wom[,1],na.rm=TRUE)
  wom2[c(1,15),1] <- tapply(wom2[,1],wom2$foo,mean,na.rm=TRUE)
  expect_equal(impute_proxy(wom,height ~ mean(height,na.rm=TRUE)*rep(1,length(height))),wom1)
  expect_equal(impute_proxy(wom,height ~ mean(height,na.rm=TRUE)),wom1)
  expect_equal(impute_proxy(wom,height ~ mean(height,na.rm=TRUE)*rep(1,length(height))|foo),wom2)
  expect_equal(impute_proxy(wom,height ~ mean(height,na.rm=TRUE)|foo),wom2)
  
})

test_that("regression tests",{
  irisNA <- iris
  irisNA[1:3,1] <- NA
  # this used to cause havoc because the ... was not passed correctly
  out <- impute_lm(irisNA, .-Sepal.Width ~ 0 + Sepal.Width, weights=1/iris$Sepal.Length)
  expect_equal(sum(is.na(out)),0)
})

