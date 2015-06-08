context("clusDropVar2")

## TODO: Rename context
## TODO: Add more tests

test_that("clusDropVar2 works", {
  d <- clusDropVar(iris, 3:5, threshold = .6, vte = "Species", xLoopsBeforePrint=1)
  expect_is(d, "list")
})
