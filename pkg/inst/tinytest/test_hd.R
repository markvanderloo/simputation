

## sequential hot deck
dat <- data.frame(
  x = c(1,2,NA,4)
  , y = c(2,NA,NA,8)
)


  
expect_equal(impute_shd(dat, x + y ~ 1,pool="univariate")
             , data.frame(x=c(1,2,2,4),y=c(2,2,2,8)))

expect_equal(impute_shd(dat, x + y ~ 1,pool="univariate", order="nocb")
             , data.frame(x=c(1,2,4,4),y=c(2,8,8,8)))

expect_equal(impute_shd(dat, x + y ~ 1,pool="multivariate")
             , data.frame(x=c(1,2,1,4),y=c(2,2,2,8)))

  
expect_equal(impute_shd(dat, x + y ~ 1,order="nocb")
             , data.frame(x=c(1,2,4,4),y=c(2,8,8,8)))

expect_equal(
    impute_shd(dat, y ~ x)
  , data.frame(x=c(1,2,NA,4),y=c(2,2,8,8))
)


## random hot deck
  
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


## knn-imputation
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
  

## pmm-imputation: test pool options
dat <- data.frame(
  x = c(1.0, 2.0, 3.0, 3.0, 3.1, 5.0)
  , y = c(0.9, 2.1,  NA,  NA, 2.4, 4.9)
  , z = c(2.2, 3.8, 4.2,  NA,  NA, 10.1)
)

expected_result <- data.frame(
  x = c(1.0, 2.0, 3.0, 3.0, 3.1, 5.0)
  , y = c(0.9, 2.1, 2.4, 2.4, 2.4, 4.9)
  , z = c(2.2, 3.8, 4.2, 4.2, 4.2, 10.1)
)
expect_equal(impute_pmm(dat, y + z ~ x, pool="univariate"), expected_result)

expected_result <- data.frame(
  x = c(1.0, 2.0, 3.0, 3.0, 3.1, 5.0)
  , y = c(0.9, 2.1, 2.4, 2.1, 2.4, 4.9)
  , z = c(2.2, 3.8, 4.2, 3.8, 4.2, 10.1)
)
expect_equal(impute_pmm(dat, y + z ~ x, pool="multivariate"), expected_result)

expected_result <- data.frame(
  x = c(1.0, 2.0, 3.0, 3.0, 3.1, 5.0)
  , y = c(0.9, 2.1, 2.1, 2.1, 2.4, 4.9)
  , z = c(2.2, 3.8, 4.2, 3.8, 3.8, 10.1)
)
expect_equal(impute_pmm(dat, y + z ~ x, pool="complete"), expected_result)

## pmm-imputation: test predictor options
dat <- iris[1:15, ]
dat[8, "Sepal.Length"] <- NA
dat[9, "Sepal.Width"] <- NA
expected_result <- dat
expected_result[8, "Sepal.Length"] <- 4.6
expected_result[9, "Sepal.Width"] <- 3.5
formula <- Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

expect_equal(impute_pmm(dat, formula, pool = "univariate"), expected_result)
expect_equal(impute_pmm(dat, formula, pool = "multivariate"), expected_result)
expect_equal(impute_pmm(dat, formula, pool = "complete"), expected_result)

expect_equal(impute_pmm(dat, formula, predictor = impute_lm, pool = "univariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_lm, pool = "multivariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_lm, pool = "complete"), expected_result)

expect_equal(impute_pmm(dat, formula, predictor = impute_rlm, pool = "univariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_rlm, pool = "multivariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_rlm, pool = "complete"), expected_result)

expect_equal(impute_pmm(dat, formula, predictor = impute_en, pool = "univariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_en, pool = "multivariate"), expected_result)
expect_equal(impute_pmm(dat, formula, predictor = impute_en, pool = "complete"), expected_result)

# Hotdeck predictors not supported for pmm.
expect_error(impute_pmm(dat, formula, predictor = impute_rhd), pattern = "Cannot use 'impute_rhd' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_shd), pattern = "Cannot use 'impute_shd' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_pmm), pattern = "Cannot use 'impute_pmm' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_knn), pattern = "Cannot use 'impute_knn' as predictor")

# Multivariate predictors not supported for pmm.
expect_error(impute_pmm(dat, formula, predictor = impute_em), pattern = "Cannot use 'impute_em' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_mf), pattern = "Cannot use 'impute_mf' as predictor")

# Proxy predictors not supported for pmm.
expect_error(impute_pmm(dat, formula, predictor = impute_proxy), pattern = "Cannot use 'impute_proxy' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_const), pattern = "Cannot use 'impute_const' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_median), pattern = "Cannot use 'impute_median' as predictor")

# Treemodel predictors not supported for pmm.
expect_error(impute_pmm(dat, formula, predictor = impute_cart), pattern = "Cannot use 'impute_cart' as predictor")
expect_error(impute_pmm(dat, formula, predictor = impute_rf), pattern = "Cannot use 'impute_rf' as predictor")

## pmm-imputation: test NA in predictor result
dat <- iris[1:15, ]
dat[1, "Petal.Length"] <- NA
dat[2, "Petal.Width"] <- NA
dat[8, "Sepal.Length"] <- NA
dat[9, "Sepal.Width"] <- NA
expected_result <- dat
expected_result[8, "Sepal.Length"] <- 4.6
expected_result[9, "Sepal.Width"] <- 3.6
formula <- Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width
expect_equal(impute_pmm(dat, formula, pool = "univariate"), expected_result)
expect_equal(impute_pmm(dat, formula, pool = "multivariate"), expected_result)
expect_equal(impute_pmm(dat, formula, pool = "complete"), expected_result)

## pmm-imputation: test many NAs, leaving 0 donors for the "complete" option.
dat <- iris[1:12, ]
dat[1:2, "Petal.Length"] <- NA
dat[3:4, "Petal.Width"] <- NA
dat[c(1, 5:7, 11:12), "Sepal.Length"] <- NA
dat[c(3, 8:12), "Sepal.Width"] <- NA
expected_result <- dat
expected_result[8, "Sepal.Length"] <- 4.6
expected_result[9, "Sepal.Width"] <- 3.6
formula <- Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

expected_result <- dat
expected_result[is.na(dat["Sepal.Length"]), "Sepal.Length"] <- c(NA, 4.4, 5.0, 4.4, 5.0, 5.0)
expected_result[is.na(dat["Sepal.Width"]), "Sepal.Width"] <- c(NA, 3.9, 3.6, 3.9, 3.9, 3.9)
expect_equal(impute_pmm(dat, formula, pool = "univariate"), expected_result)
expected_result <- dat
expected_result[is.na(dat["Sepal.Length"]), "Sepal.Length"] <- c(NA, 4.4, 5.0, 4.4, NA, NA)
expected_result[is.na(dat["Sepal.Width"]), "Sepal.Width"] <- c(NA, 3.9, 3.6, 3.9, NA, NA)
expect_equal(impute_pmm(dat, formula, pool = "multivariate"), expected_result)
expected_result <- dat
expect_equal(impute_pmm(dat, formula, pool = "complete"), expected_result)


## grouped imputation
pools <- c("complete","univariate","multivariate")
## random hot deck
dat <- data.frame(
  x = c(1,2,NA)
  , y = 1:3
  , z = c("a","b","b")
)

for ( pool in  pools ){
  expect_equal(impute_rhd(dat, x ~ z,pool=pool)[,1],c(1,2,2))
  expect_equal(impute_rhd(dat, x ~ 1|z,pool=pool)
    , impute_rhd(dat, x ~ z,pool=pool))
}

# sequential hot deck
dat <- data.frame(
  x = c(1,2,NA,4)
  , z = c("a","b","a","b")
)
for (pool in pools){
 expect_equal(impute_shd(dat, x ~ 1,pool=pool)[,1],c(1,2,2,4))
 expect_equal(impute_shd(dat, x~1|z, pool=pool)[,1],c(1,2,1,4))
}

# predictive mean matching
dat <- data.frame(
   x = c(1,2.2,3,4,1,2,3,4)
  , y = c(1,NA,3,4,1,2,3,4)
  , z = rep(c("a","b"),each=4))
for ( pool in pools){
  expect_equal(impute_pmm(dat,y~x,predictor=impute_lm,pool=pool)[2,2], 2)
  expect_equal(impute_pmm(dat,y~x|z,predictor=impute_lm,pool=pool)[2,2],3)
}

# knn imputation
dat <- data.frame(
  x = c(1,2,3,1,2,3)
  , y = c(1,NA,3,1,2,3)
  , z = rep(c("a","b"),each=3)
)
for (pool in pools){
  expect_equal(impute_knn(dat, y ~ x, k=1,pool=pool)[2,2], 2)
  expect_equal(impute_knn(dat, y ~ x | z, k=1,pool=pool)[2,2], 1)
}

  

## hotdeck imputation with VIM-backend
if ( suppressPackageStartupMessages(require("VIM", quietly=TRUE)) ){
  ## random hotdeck with VIM backend
  dat <- data.frame(
    foo = c(2,NA,4)
    , bar = c(NA,NA,8)
  )
  expect_false(anyNA(impute_rhd(dat, foo ~ 1, backend="VIM")["foo"]) )
  expect_false(anyNA(impute_rhd(dat, . ~ 1, backend="VIM")) )
  
  
  ## sequential hotdeck with VIM backend
  dat <- data.frame(
    id = 0:3
    ,foo = c(1,2,NA,4)
    , bar = c(2,NA,NA,8)
  )
  expect_equal(
    impute_shd(dat, .~ id, backend="VIM")
    , impute_shd(dat, .~ id, pool="univariate")
  )
  
  
  ## knn hotdeck with VIM backend
  dat <- data.frame(x = c(NA,2,4,5), y = c(6,7,NA,10))
  options(gd_num_thread = 1L)
  
  expect_equal(
    impute_knn(dat, . ~ ., k=1)
    , impute_knn(dat, .~.,k=1, backend="VIM") 
  )
  
}



