context("lsos")

## TODO: Rename context
## TODO: Add more tests

test_that("lsos works", {
  data(iris)
  out <- lsos()
  expect_is(out, "data.frame")
  expect_equal(ncol(out), 4)
})
