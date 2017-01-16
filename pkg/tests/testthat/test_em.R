
context("EM imputation")

test_that("EM imputation",{
  x <- iris[-5]
  x[1:3,1] <- x[5:7,2] <- NA
  expect_equal(sum(is.na(impute_em(x, ~ . ))),0)
  expect_equal(sum(is.na(impute_em(x, ~ . - Sepal.Width))),3)
  
  # grouped imputation
  x <- iris
  x[1:3,1] <- x[5:7,2] <- NA
  expect_equal(sum(is.na(impute_em(x, ~ . -Species))),0)
  expect_equal(sum(is.na(impute_em(x, ~ . -Species | Species))),0)
  expect_false(identical(
    impute_em(x, ~ . - Species)
    , impute_em(x, ~ . - Species|Species)
  ))
  
})

test_that("EMB imputation", {
  dd <- data.frame( 
    x = rnorm(20), y=rnorm(20),z=rnorm(20)
  )
  dd[1:3,1] <- dd[4:6,2] <- dd[7:9,3] <- NA
  d1 <- impute_emb(dd, x ~ y)
  expect_equal(sum(is.na(d1[,3])),3)
  expect_equal(sum(is.na(d1[,1:2])),0)
  d1 <- impute_emb(dd,  ~ x + y)
  expect_equal(sum(is.na(d1[,3])),3)
  expect_equal(sum(is.na(d1[,1:2])),0)
  
  # grouped imputation
  x <- iris
  x[1:3,1] <- x[5:7,2] <- NA
  expect_equal(sum(is.na(impute_emb(x, ~ . -Species))),0)
  expect_equal(sum(is.na(impute_emb(x, ~ . -Species | Species))),0)
})

