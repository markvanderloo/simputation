

d <- data.frame(
    `a col` = c(1,3,5,7)
  , `b col` = c(NA,2,5,7)
  , check.names=FALSE # so R doesn't replace the space with a '.'.
)

expect_equal(simputation:::get_imputed(`a col` ~ `b col`,d),"a col")
expect_equal(simputation:::get_predictors(`a col` ~ `b col`,d), "b col")

# from the original bug report of Miles McBain
expect_silent(impute_knn(d, `a col` ~ `b col`, k=3))


