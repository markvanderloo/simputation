
context("EM imputation")

test_that("The right stuff gets imputed", {
  dd <- data.frame( 
    x = rnorm(20), y=rnorm(20),z=rnorm(20)
  )
  dd[1:3,1] <- dd[4:6,2] <- dd[7:9,3] <- NA
  d1 <- impute_em(dd, x ~ y)
  expect_equal(sum(is.na(d1[,3])),3)
  expect_equal(sum(is.na(d1[,1:2])),0)
  d1 <- impute_em(dd,  ~ x + y)
  expect_equal(sum(is.na(d1[,3])),3)
  expect_equal(sum(is.na(d1[,1:2])),0)
  
})

