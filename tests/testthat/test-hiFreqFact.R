context("hiFreqFact")

## TODO: Rename context
## TODO: Add more tests

test_that("cat_names works", {
  data(iris)
  levels(hiFreqFact(iris$Species, y = 25))
})
