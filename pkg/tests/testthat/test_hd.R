
context("Hot deck imputation")

test_that("sequential hot deck",{
  dat <- data.frame(
    x = c(1,2,NA,4)
    , y = c(2,NA,NA,8)
  )
  
  expect_equal(impute_shd(dat, x + y ~ 1)
               , data.frame(x=c(1,2,2,4),y=c(2,2,2,8))
               ,info="vanilla single hot deck")
  
  expect_equal(
    impute_shd(dat, y ~ x)
               , data.frame(x=c(1,2,NA,4),y=c(2,2,8,8))
               , info="impute y, sorted by x"
               )
})

