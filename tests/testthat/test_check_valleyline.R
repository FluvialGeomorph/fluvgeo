library(fgm)
context("check_valleyline")

test_that("check valleyline", {
  expect_true(check_valleyline(fgm::sin_valleyline_sp))
})

test_that("check not valleyline", {
  expect_error(check_valleyline(fgm::sin_features_sp))
  expect_error(check_valleyline(fgm::sin_loop_points_sp))
  expect_error(check_valleyline(fgm::sin_bankline_points_sp))
})
