library(fluvgeo)
context("check_valleyline_points")

test_that("check valleyline_points", {
  expect_true(check_valleyline_points(fluvgeo::sin_valleyline_points_sf))
})

test_that("not valleyline points points", {
  expect_error(check_valleyline_points(fluvgeo::sin_loop_points_sf))
  expect_error(check_valleyline_points(fluvgeo::sin_bankline_points_sf))
})
