
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

})



