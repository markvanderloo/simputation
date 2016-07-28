
context("Hot deck imputation")

test_that("sequential hot deck",{
  dat <- data.frame(
    x = c(1,2,NA,4)
    , y = c(2,NA,NA,8)
  )
  
  expect_equal(impute_shd(dat, x + y ~ 1)
               , data.frame(x=c(1,2,2,4),y=c(2,2,2,8))
               ,info="unsorted single hot deck")
  
  expect_equal(impute_shd(dat, x + y ~ 1,order="nocb")
               , data.frame(x=c(1,2,4,4),y=c(2,8,8,8))
               ,info="unsorted single hot deck")
  
  expect_equal(
    impute_shd(dat, y ~ x)
               , data.frame(x=c(1,2,NA,4),y=c(2,2,8,8))
               , info="impute y, sorted by x"
               )
})


test_that("random hot deck",{
  
  dat <- data.frame(
    x = c(2,NA,4)
    , y = c(NA,NA,8)
  )
  expect_equal(impute_rhd(dat, y ~ 1),data.frame(x=c(2,NA,4),y=rep(8,3))) 
  expect_true(!is.na(sum(impute_rhd(dat, x + y ~ 1))) )
  expect_true(!is.na(sum(impute_rhd(dat, x + y ~ 1,pool="multivariate"))) )
  dat$foo <- letters[1:3]
  expect_equal(impute_rhd(dat,x+y~foo),dat)
  expect_equal(impute_rhd(dat, x+y~foo,pool="multivariate"),dat)
})

