context("lsos")

## TODO: Rename context
## TODO: Add more tests

test_that("lsos works", {
  data(iris)
  out <- lsos()
  expect_is(out, 'data.frame')
  expect_equal(dim(out), c(3, 4))
})
