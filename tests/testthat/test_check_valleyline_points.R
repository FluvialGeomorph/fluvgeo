library(fgm)
context("check_valleyline_points")

test_that("check valleyline_points", {
  expect_true(check_valleyline_points(fgm::sin_valleyline_points_sp))
})

test_that("not valleyline points points", {
  expect_error(check_valleyline_points(fgm::sin_loop_points_sp))
  expect_error(check_valleyline_points(fgm::sin_bankline_points_sp))
})
