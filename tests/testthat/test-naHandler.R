context("naHandler")

## TODO: Rename context
## TODO: Add more tests

test_that("naHandler works", {
  data(iris)
  nas <- lapply(iris, function(x) naHandler(x))
  expect_equal(nas, as.list(iris))
})
