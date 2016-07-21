
context("Formula parsing")
test_that("formula parsing",{
  expect_equal(get_predicted(x ~ y, c("x","y")),"x")
  expect_equal(get_predicted( . ~ y,c("x","y","z")),c("x","z"))
  expect_error(get_predicted( x ~ y, c("y","z")),regexp="Trying")
  expect_error(get_predicted( x ~ x, c("x","y")),regexp="Using")
})

