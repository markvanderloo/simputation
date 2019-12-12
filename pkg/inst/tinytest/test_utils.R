

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

long_formula <- this_is_a_formula_with_long_variables ~
  the_test_is_checking_if_deparse_will_return +
  multiple_strings_or_not
expect_equal(length(base::deparse(long_formula)), 2)
expect_equal(length(simputation:::deparse(long_formula)), 1)
