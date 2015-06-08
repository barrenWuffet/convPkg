context("naHandler")

## TODO: Rename context
## TODO: Add more tests

test_that("naHandler works with no NAs", {
  data(iris)
  nas <- lapply(iris, function(x) naHandler(x))
  expect_equal(nas, as.list(iris))
})

test_that("naHandler works with NAs", {
  data(iris)
  x <- iris
  x[1,] <- NA
  nas <- lapply(x, function(x) naHandler(x))
  expect_is(nas, 'list')
})
