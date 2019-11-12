library(fluvgeo)
context("check_loop_points")

test_that("check loop points", {
  expect_true(check_loop_points(fluvgeo::sin_loop_points_sp))
})

test_that("check not loop points", {
  expect_error(check_loop_points(fluvgeo::sin_banklines_sp))
  expect_error(check_loop_points(fluvgeo::sin_features_sp))
  expect_error(check_loop_points(fluvgeo::sin_riffle_channel_points_sp))
})
