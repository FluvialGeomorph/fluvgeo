library(fluvgeo)
context("check_banklines")

test_that("check banklines", {
  expect_true(check_banklines(fluvgeo::sin_banklines_sp))
})

test_that("not bankline points", {
  expect_error(check_banklines(fluvgeo::sin_flowline_sp))
  expect_error(check_banklines(fluvgeo::sin_loop_points_sp))
  expect_error(check_banklines(fluvgeo::sin_bankline_points_sp))
})
