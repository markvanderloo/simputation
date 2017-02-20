
context("foretell")

test_that("foretell returns correct type",{
  m <- lm(Sepal.Length ~ ., data=iris)
  expect_true(inherits(foretell(m,iris),"numeric" ))
  m <- rpart::rpart(Species ~ ., data=iris)
  expect_true(inherits(foretell(m,iris),"factor"))
  iris$foo <- sample(c(TRUE,FALSE),150,replace=TRUE)
  m <- stats::glm(foo ~ .-Species,data=iris, family="binomial")
  expect_true(inherits(foretell(m,newdata=iris),"logical"))
  
  if(requireNamespace("glmnet",quietly=TRUE)){
    iris$foo <- sample(c(0,1),150,replace=TRUE)
    m <- glmnet::glmnet(x=as.matrix(iris[1:4]), y=iris$foo, fam="bin")
    
    #foretell(m,newdata=iris)
  }
  
})


