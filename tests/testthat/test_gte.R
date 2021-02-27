library(fluvgeo)
context("gte")

# create a numeric vector
x <- seq(-2, 2, 0.1)

test_that("check values", {
  expect_true(all(gte(x, 0) >= 0))
  expect_true(all(gte(x, 1) >= 1))
})

test_that("check data type", {
  expect_error(gte(letters, 0) >= 0)
  expect_error(gte(x, "a"))
})
