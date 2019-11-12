library(fluvgeo)
context("check_valleyline")

test_that("check valleyline", {
  expect_true(check_valleyline(fluvgeo::sin_valleyline_sp))
})

test_that("check not valleyline", {
  expect_error(check_valleyline(fluvgeo::sin_features_sp))
  expect_error(check_valleyline(fluvgeo::sin_loop_points_sp))
  expect_error(check_valleyline(fluvgeo::sin_bankline_points_sp))
})
