library(fluvgeo)
context("check_features")

# sp
test_that("check features", {
  expect_true(check_features(fluvgeo::sin_features_sp))
})

test_that("not features points", {
  expect_error(check_features(fluvgeo::sin_flowline_sp))
  expect_error(check_features(fluvgeo::sin_loop_points_sp))
  expect_error(check_features(fluvgeo::sin_bankline_points_sp))
})

# sf
features_fc   <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "feature_dataset/features")
features_sf  <- fluvgeo::fc2sf(features_fc)

test_that("check features", {
  expect_true(check_features(features_sf))
})
