
context("Utilities")

test_that("presence of suggested packages",{
  capture.output(simputation_suggests())
})

test_that("stop at grouping error",{
  iris[1,c(1,5)] <- NA
  expect_error(impute_lm(iris,Sepal.Width ~ 1 | Species))
})

test_that("na.roughfix, na.rpart",{
  a <- iris
  a[1:3,1] <- a[3:7,5] <- NA
  expect_equal(sum(is.na(na.roughfix(a))),0)
  expect_equal(a,na.rpart(a))
})
