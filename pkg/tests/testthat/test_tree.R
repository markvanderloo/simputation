
context("Tree models")
# some testing is also done in test_lm
test_that("CART models",{
  
  # impute categorical data
  dat <- data.frame(x=rep(c("a","b"),each=50), y = 1:100)
  dat[c(1,10),1] <- NA
  expect_equal(
    impute_cart(dat, x ~ y)
  , data.frame(x=rep(c("a","b"),each=50), y = 1:100)
  )
  # impute logical data
  dat <- data.frame(x=rep(c(TRUE,FALSE),each=50), y = 1:100)
  dat[c(1,10),1] <- NA

  expect_equal(
  impute_cart(dat, x ~ y)
  , data.frame(x=rep(c(TRUE,FALSE),each=50), y = 1:100)
  )
  
})

