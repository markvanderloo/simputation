
test_that("presence of suggested packages",{
  capture.output(simputation_suggests())
})

test_that("stop at grouping error",{
  iris[1,c(1,5)] <- NA
  expect_error(impute_lm(iris,Sepal.Width ~ 1 | Species))
})
