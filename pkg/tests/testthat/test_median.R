
context("Median imputation")

test_that("correct medians are imputed",{
  set.seed(1)
  iris2 <- iris[sample(150), ]
  iris2[1,1] <- iris2[1:3, 2] <- NA
  
  out <- simputation::impute_median(iris2, . ~ Species)
  expect_equal(out[1:3,2]c(3.4,2.8,2.8))
  
})
