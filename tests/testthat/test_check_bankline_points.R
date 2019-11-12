library(fluvgeo)
context("check_bankline_points")

test_that("check bankline points", {
  expect_true(check_bankline_points(fluvgeo::sin_bankline_points_sp))
})

test_that("not bankline points", {
  expect_error(check_bankline_points(fluvgeo::sin_flowline_sp))
  expect_error(check_bankline_points(fluvgeo::sin_loop_points_sp))
  expect_error(check_bankline_points(fluvgeo::sin_banklines_sp))
})
