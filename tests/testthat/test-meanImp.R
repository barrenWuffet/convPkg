context("meanImp")

## TODO: Rename context
## TODO: Add more tests

test_that("meanImp works", {
  data(iris)
  d <- meanImp(iris$Petal.Length)
  expect_equal(d, iris$Petal.Length)
})
