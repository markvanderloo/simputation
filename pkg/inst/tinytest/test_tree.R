# option to check whether R runs with the new stringsAsFactors policy
#options(stringsAsFactors=FALSE)

## Tree models
# some testing is also done in test_lm
## CART models
  
# impute categorical data
dat <- data.frame(x=rep(c("a","b"),each=50), y = 1:100, stringsAsFactors=TRUE)
dat[c(1,10),1] <- NA
expect_equal(
  impute_cart(dat, x ~ y)
, data.frame(x=rep(c("a","b"),each=50), y = 1:100, stringsAsFactors=TRUE)
)
# impute logical data
dat <- data.frame(x=rep(c(TRUE,FALSE),each=50), y = 1:100)
dat[c(1,10),1] <- NA

expect_equal(
impute_cart(dat, x ~ y)
, data.frame(x=rep(c(TRUE,FALSE),each=50), y = 1:100)
)


dat <- data.frame(
  x = rnorm(100)
  , y = sample(c(TRUE,FALSE),100,replace=TRUE)
  , z = sample(letters[1:4],100,replace=TRUE)
  , p = runif(100)
  , stringsAsFactors=TRUE
)
dat[1:3,"x"] <- NA
dat[4:6,"y"] <- NA
dat[6:8,"z"] <- NA
expect_true(is.logical(impute_cart(dat,y~p)[,'y']))
expect_true(is.factor(impute_cart(dat,z~p)[,'z']))
expect_true(is.numeric(impute_cart(dat,x~p)[,'x']))

#test behaviour with different options of CP-passing. 
expect_true( !any(is.na(impute_cart(dat, x~p, cp=0.01))[,'x'] ) ) 
expect_true( !any(is.na(impute_cart(dat, x + y~p, cp=0.01))[,'x'] ) ) 
expect_error( impute_cart(dat, x + y + z ~ p, cp=c(0.01,0.1))[,'x']  ) 
expect_true( !any(is.na(impute_cart(dat, x + y~p, cp=rep(0.01,2)))[,'x'] ) ) 

# Test impute_all = TRUE
dat <- iris[1:10, ]
dat[1:2,1] <- NA 
expected_result <- dat
expected_result[1:2,1] <- 4.825
expect_equal(impute_cart(dat, Sepal.Length ~ Sepal.Width), expected_result)
expected_result <- dat
expected_result[ ,1] <- 4.825
expect_equal(impute_cart(dat, Sepal.Length ~ Sepal.Width, impute_all = TRUE), expected_result)


if (requireNamespace("randomForest",quietly = TRUE)){
  ## RandomForest imputation
    dat <- iris
    dat[1:3,1] <- dat[4:7,5] <- NA 
    expect_true(!any(is.na(impute_rf(dat, Species + Sepal.Length ~ Sepal.Width))))
    expect_true(!any(is.na(impute_rf(dat, Species + Sepal.Length ~ Sepal.Width, add_residual="observed"))))
    expect_true(!any(is.na(impute_rf(dat, Species + Sepal.Length ~ Sepal.Width, add_residual="normal"))))
  
    dat <- iris
    dat[sample(1:nrow(iris),25),"Species"] <- NA
    expect_true(!any(is.na(impute_rf(dat, Species ~.))))
    
}

## grouped imputation
dat <- data.frame(
  x = 1:100
  , y = rep(LETTERS[1:4],times=25)
  , z = rep(c("a","b"),each=50)
  , stringsAsFactors=TRUE
)
dat[2,1] <- NA
expect_true(impute_cart(dat, x ~ y|z)[2,1] < impute_cart(dat, x ~ y)[2,1])
if (requireNamespace("randomForest",quietly = TRUE)){
  expect_true(impute_rf(dat, x ~ y|z)[2,1] < impute_rf(dat, x ~ y)[2,1])
}

# Test impute_all = TRUE
dat <- iris[1:10, ]
dat[1:2,1] <- NA 
expected_result <- dat
expected_result[1:2,1] <- c(4.831, 4.567)
set.seed(0)
result <- impute_rf(dat, Sepal.Length ~ Sepal.Width)
expect_equal(result, expected_result, tolerance = 1e-2)
expected_result <- dat
expected_result[ ,1] <- c(4.831, 4.567, 4.720, 4.721, 4.977, 5.191, 4.811, 4.811, 4.567, 4.721)
set.seed(0)
result <- impute_rf(dat, Sepal.Length ~ Sepal.Width, impute_all = TRUE)
expect_equal(result, expected_result, tolerance = 1e-2)
