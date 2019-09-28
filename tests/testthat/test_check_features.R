library(fgm)
context("check_features")

test_that("check features", {
  expect_true(check_features(fgm::sin_features_sp))
})

test_that("not features points", {
  expect_error(check_features(fgm::sin_flowline_sp))
  expect_error(check_features(fgm::sin_loop_points_sp))
  expect_error(check_features(fgm::sin_bankline_points_sp))
})
