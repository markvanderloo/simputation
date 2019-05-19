

## presence of suggested packages
dummy <- capture.output(simputation_suggests())


## stop at grouping error
haha <- iris
haha[1,c(1,5)] <- NA
expect_error(impute_lm(haha, Sepal.Width ~ 1 | Species))

## na.roughfix, na.rpart
  a <- iris
  a[1:3,1] <- a[3:7,5] <- NA
  expect_equal(sum(is.na(na.roughfix(a))),0)
  expect_equal(a,na.rpart(a))

