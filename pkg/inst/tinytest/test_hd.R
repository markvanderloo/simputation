

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
  

## pmm-imputation
dat <- data.frame(x=c(1,2,2,4),y=c(0.9,2.1,NA,4.2))
expect_equal(
  impute_pmm(dat, y ~ x, pool="univariate")
  , data.frame(x=c(1,2,2,4),y=c(0.9, 2.1,2.1,4.2))
)
dat <- data.frame(x=c(1.0, 2.0, 3.0, 3.0, 5.0)
                , y=c(0.9, 2.1, NA,  NA,  4.9)
                , z=c(2.2, 3.8, 4.2, NA, 10.1))
expect_equal(
    impute_pmm(dat, y + z ~ x, pool="multivariate")
  , data.frame(x=dat$x
               , y=c(0.9, 2.1, 2.1, 2.1, 4.9)
               , z=c(2.2, 3.8, 4.2, 3.8, 10.1)) 
)
expect_equal(
    impute_pmm(dat, y + z ~ x, pool="complete")
  , data.frame(x=dat$x
               , y=c(0.9, 2.1, 2.1, 2.1, 4.9)
               , z=c(2.2, 3.8, 4.2, 3.8, 10.1)) 
)
  

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
    impute_shd(dat, .~id, backend="VIM")
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



