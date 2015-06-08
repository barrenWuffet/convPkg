context("naHandler")

## TODO: Rename context
## TODO: Add more tests

test_that("naHandler works", {
  data(iris)
  nas <- lapply(iris[,c('Petal.Length','Petal.Width','Species')], function(x) naHandler(x))
  expect_equal(nas, iris[,c('Petal.Length','Petal.Width','Species')])
})
