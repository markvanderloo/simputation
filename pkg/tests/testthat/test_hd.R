
context("Hot deck imputation")

test_that("sequential hot deck",{
  dat <- data.frame(
    x = c(1,2,NA,4)
    , y = c(2,NA,NA,8)
  )

  
    
  expect_equal(impute_shd(dat, x + y ~ 1,pool="univariate")
               , data.frame(x=c(1,2,2,4),y=c(2,2,2,8))
               ,info="unsorted univariate single hot deck")

  expect_equal(impute_shd(dat, x + y ~ 1,pool="univariate", order="nocb")
               , data.frame(x=c(1,2,4,4),y=c(2,8,8,8))
               ,info="unsorted univariate single NOCB hot deck")
  
  
  expect_equal(impute_shd(dat, x + y ~ 1,pool="multivariate")
               , data.frame(x=c(1,2,1,4),y=c(2,2,2,8))
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
  expect_equal(impute_rhd(dat, y ~ 1),data.frame(x=c(2,NA,4),y=rep(8,3)),pool="complete") 
  expect_true(!is.na(sum(impute_rhd(dat, x + y ~ 1))) )
  expect_true(!is.na(sum(impute_rhd(dat, x + y ~ 1,pool="multivariate"))) )
  expect_true(!is.na(sum(impute_rhd(dat, x + y ~ 1,pool="univariate"))) )
  dat$foo <- letters[1:3]
  expect_equal(impute_rhd(dat,x+y~foo),dat)
  expect_equal(impute_rhd(dat, x+y~foo,pool="multivariate"),dat)
})

test_that("knn-imputation",{
  dat <- data.frame(x = c(NA,2,4,5), y = c(6,7,NA,10))
  options(gd_num_thread = 1L)

  expect_equal(
    impute_knn(dat, x + y ~ x + y, pool = "complete", k=1)
  , data.frame(x=c(2,2,4,5),y=c(6,7,10,10)) 
  )
   
   expect_equal(
     impute_knn(dat, x + y ~ x + y, pool = "univariate", k=1)
     , data.frame(x=c(2,2,4,5),y=c(6,7,10,10)) 
   )
   
   expect_equal(
     impute_knn(dat, x + y ~ x + y, pool = "multivariate", k=1)
     , data.frame(x=c(2,2,4,5),y=c(6,7,10,10)) 
   )
  
})


test_that("pmm-imputation",{
  dat <- data.frame(x=c(1,2,2,4),y=c(0.9,2.1,NA,4.2))
  expect_equal(
    impute_pmm(dat, y ~x)
    , data.frame(x=c(1,2,2,4),y=c(0.9, 2.1,2.1,4.2))
  )
  
  
})






