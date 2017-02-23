
context("Impute with previously estimated models")
test_that("stuff gets imputed",{
  irisNA <- iris
  irisNA[1:3,1] <- irisNA[4:6,2] <- NA
  out <- impute(irisNA
        , Sepal.Length  ~ lm(Sepal.Length ~ Petal.Width + Species, data=iris))
  expect_equal(sum(is.na(out$Sepal.Length)),0L)  
  out <- impute(irisNA
  , Sepal.Length + Sepal.Width ~ lm(Sepal.Length ~ Petal.Width + Species, data=iris))
  
  expect_equal(sum(is.na(out)),0L)  

  m <- lm(Sepal.Length ~ Petal.Width,data=iris)
  out <- impute_(irisNA, variables = "Sepal.Length",model=m)
  expect_equal( sum(is.na(out)),3)
  out <- impute_(irisNA, variables = c("Sepal.Length","Sepal.Width"),model=m)
  expect_equal( sum(is.na(out)),0)
  
  
})



