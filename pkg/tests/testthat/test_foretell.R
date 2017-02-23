
context("foretell")

test_that("foretell returns correct type",{
  
  m <- lm(Sepal.Length ~ ., data=iris)
  expect_true(inherits(foretell(m,iris),"numeric" )
              , info="lm numeric type")
  
  m <- rpart::rpart(Species ~ ., data=iris)
  expect_true(inherits(foretell(m,iris),"factor")
              , info="rpart factor type")
  
  iris$foo <- sample(c(TRUE,FALSE),150,replace=TRUE)
  m <- stats::glm(foo ~ .-Species,data=iris, family="binomial")
  expect_true(inherits(foretell(m,newdata=iris),"logical")
              , info = "glm binomial type")

  m <- rpart::rpart(foo ~ ., data=iris, method="class")
  expect_true(inherits(foretell(m, newdata=iris),"logical")
              ,info="rpart logical type")  
  iris[1:3,1] <- NA
  m <- rpart::rpart(foo ~ ., data=iris, method="class")
  expect_true(inherits(foretell(m, newdata=iris),"logical")
              ,info="rpart logical type (with predicted NA)")  
  iris[1:3,1] <- NA
})


