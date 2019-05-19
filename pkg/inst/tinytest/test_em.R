library(simputation)

if ( requireNamespace("norm",quietly = TRUE) ){
  ## EM imputation
  x <- iris[-5]
  x[1:3,1] <- x[5:7,2] <- NA
  expect_equal(sum(is.na(impute_em(x, ~ . ))),0)
  expect_equal(sum(is.na(impute_em(x, ~ . - Sepal.Width))),3)
  
  x <- iris[-5]
  x[1:3,1] <- x[4:7,4] <- NA 
  d1 <- impute_em(x, Sepal.Length ~ .)
  expect_equal(sum(is.na(d1[,1])),0)
  expect_equal(sum(is.na(d1[,4])),4)
  
#  x <- iris
#  x[1:4,1] <- x[3:7,5] <- NA
#  expect_warning(impute_em(x, Sepal.Length ~ .))
  
  # grouped imputation
  x <- iris
  x[1:3,1] <- x[5:7,2] <- NA
  expect_equal(sum(is.na(impute_em(x, ~ . -Species))),0)
  expect_equal(sum(is.na(impute_em(x, ~ . -Species | Species))),0)
  expect_false(identical(
    impute_em(x, ~ . - Species)
    , impute_em(x, ~ . - Species|Species)
  ))
    
}



if ( requireNamespace("missForest",quietly=TRUE) ){
    ## missForest imputation
    dat <- iris
    dat[1:3,1] <- dat[4:7,5] <- NA 
    expect_true(!any(is.na(impute_mf(dat, Species + Sepal.Length ~ Sepal.Width))))
    d1 <- impute_mf(dat, Sepal.Length ~ .)
    expect_equal(sum(is.na(d1[,1])),0)
    expect_equal(sum(is.na(d1[,5])),4)
}

