library(fluvgeo)
library(dplyr)
context("planform")

pf <- planform(fluvgeo::sin_bankline_points_sp)

test_that("check data structure", {
  expect_true(is.data.frame(pf))
  expect_true("bend_num" %in% colnames(pf))
  expect_true("loop" %in% colnames(pf))
  expect_true("bend" %in% colnames(pf))
  expect_true("bend_POINT_X" %in% colnames(pf))
  expect_true("bend_POINT_Y" %in% colnames(pf))
  expect_true("loop_POINT_X" %in% colnames(pf))
  expect_true("loop_POINT_Y" %in% colnames(pf))
  expect_true("bend_radius" %in% colnames(pf))
  expect_true("meander_length" %in% colnames(pf))
  expect_true("meander_width" %in% colnames(pf))
  expect_true(is.numeric(pf$bend_num))
  expect_true(is.integer(pf$loop))
  expect_true(is.integer(pf$bend))
  expect_true(is.numeric(pf$bend_POINT_X))
  expect_true(is.numeric(pf$bend_POINT_Y))
  expect_true(is.numeric(pf$loop_POINT_X))
  expect_true(is.numeric(pf$loop_POINT_Y))
  expect_true(is.numeric(pf$bend_radius))
  expect_true(is.numeric(pf$meander_length))
  expect_true(is.numeric(pf$meander_width))
})

test_that("check number of records", {
  assert_that(length(pf$bend_num) == length(dplyr::count(pf, loop, bend)$loop),
              msg = "incorrect number of bends")
})
