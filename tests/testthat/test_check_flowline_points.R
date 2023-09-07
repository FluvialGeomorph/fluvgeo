library(fluvgeo)
context("check_flowline_points")

test_that("check flowline points", {
  expect_true(check_flowline_points(fluvgeo::sin_flowline_points_sf))
})

test_that("not flowline points", {
  expect_error(check_flowline_points(fluvgeo::sin_flowline_sf))
  expect_error(check_flowline_points(fluvgeo::sin_loop_points_sf))
  expect_error(check_flowline_points(fluvgeo::sin_banklines_sf))
})
