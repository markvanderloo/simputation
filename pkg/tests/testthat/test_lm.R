
context("Formula parsing")
test_that("formula parsing",{
  expect_equal(get_predicted(x ~ y, c("x","y")),"x")
  expect_equal(get_predicted( . ~ y,c("x","y","z")),c("x","z"))
  expect_error(get_predicted( x ~ y, c("y","z")),regexp="Trying")
  expect_error(get_predicted( x ~ x, c("x","y")),regexp="Using")
  expect_equal(get_predictors( x ~ y,c("x","y")),"y")
  expect_error(get_predictors( x ~ y, "x"),regexp="not found")
})

context("Imputation")
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
  
  expect_equal(sum(is.na(impute_proxy(irisNA, Sepal.Length ~ Sepal.Width))), 7)  
  expect_equal(sum(is.na(impute_proxy(irisNA, Sepal.Length + Sepal.Width ~ Petal.Width))),3)
  expect_error(impute_proxy(irisNA, Sepal.Length ~ Sepal.Width + Petal.Length)
               ,regex="Need")
  # Try imputing a column where everything is missing (so models don't fit)
  irisNA$foo <- NA_real_
  for ( f in list(impute_lm, impute_rlm, impute_median) ){
    expect_warning(f(irisNA, foo ~ 1))
  }
  
})

 # o <- impute_lm(irisNA,Sepal.Length ~ 1)
 # o <- impute_rlm(irisNA,. ~ 1)
 # o <- impute_const(irisNA,. ~ 1)
 # o <- impute_median(irisNA,. ~ 1)

# o <- impute_lm(irisNA,Sepal.Length ~ 1,add_residual = "normal")
# o <- impute_rlm(irisNA,. ~ 1,add_residual = "normal")
# o <- impute_const(irisNA,. ~ 1,add_residual = "normal")
# o <- impute_median(irisNA,Sepal.Width ~ 1,add_residual = "normal")

 
# 
 # o <- impute_lm(irisNA,. ~ 1)
 # o <- impute_rlm(irisNA,. ~ 1)
 # o <- impute_const(irisNA,. ~ 1)
 # o <- impute_median(irisNA,. ~ 1)
