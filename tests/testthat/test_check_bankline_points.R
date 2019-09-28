library(fgm)
context("check_bankline_points")

test_that("check bankline points", {
  expect_true(check_bankline_points(fgm::sin_bankline_points_sp))
})

test_that("not bankline points", {
  expect_error(check_bankline_points(fgm::sin_flowline_sp))
  expect_error(check_bankline_points(fgm::sin_loop_points_sp))
  expect_error(check_bankline_points(fgm::sin_banklines_sp))
})
