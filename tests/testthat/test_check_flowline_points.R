library(fluvgeo)
context("check_flowline_points")

test_that("check flowline points", {
  expect_true(check_flowline_points(fluvgeo::sin_flowline_points_sp))
})

test_that("not flowline points", {
  expect_error(check_flowline_points(fluvgeo::sin_flowline_sp))
  expect_error(check_flowline_points(fluvgeo::sin_loop_points_sp))
  expect_error(check_flowline_points(fluvgeo::sin_banklines_sp))
})
