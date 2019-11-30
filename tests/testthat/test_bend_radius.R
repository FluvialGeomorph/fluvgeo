library(fluvgeo)
context("bend_radius")

bankline_points <- fluvgeo::sin_bankline_points_sp

br <- bend_radius(bankline_points)

test_that("check bend_radius data structure", {
  expect_true(is.data.frame(br))
  expect_true("bend_num" %in% colnames(br))
  expect_true("loop" %in% colnames(br))
  expect_true("bend" %in% colnames(br))
  expect_true("bend_POINT_X" %in% colnames(br))
  expect_true("bend_POINT_Y" %in% colnames(br))
  expect_true("bend_radius" %in% colnames(br))
  expect_true(is.numeric(br$bend_num))
  expect_true(is.integer(br$loop))
  expect_true(is.integer(br$bend))
  expect_true(is.numeric(br$bend_POINT_X))
  expect_true(is.numeric(br$bend_POINT_Y))
  expect_true(is.numeric(br$bend_radius))
})
