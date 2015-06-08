context("rankTab")

## TODO: Rename context
## TODO: Add more tests

test_that("rankTab works", {
  data(iris)
  d <- rankTab(iris$Species)
  expect_is(d, "data.frame")
  expect_equal(dim(d), c(3,2))
  expect_equal(d$Freq, rep(50, 3))
})
