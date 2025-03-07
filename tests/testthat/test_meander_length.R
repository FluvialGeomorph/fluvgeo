library(fluvgeo)
context("meander_length")

bankline_points <- fluvgeo::sin_bankline_points_sf

ml <- meander_length(bankline_points = bankline_points)

test_that("check meander_length output data structure", {
  expect_true(is.data.frame(ml))
  expect_true("loop" %in% colnames(ml))
  expect_true("bank_POINT_X" %in% colnames(ml))
  expect_true("bank_POINT_Y" %in% colnames(ml))
  expect_true("bank_POINT_M" %in% colnames(ml))
  expect_true("DEM_Z" %in% colnames(ml))
  expect_true("valley_POINT_X" %in% colnames(ml))
  expect_true("valley_POINT_Y" %in% colnames(ml))
  expect_true("valley_POINT_M" %in% colnames(ml))
  expect_true("downstream_x" %in% colnames(ml))
  expect_true("downstream_y" %in% colnames(ml))
  expect_true("meander_length" %in% colnames(ml))
  expect_true(is.integer(ml$loop))
  expect_true(is.numeric(ml$bank_POINT_X))
  expect_true(is.numeric(ml$bank_POINT_Y))
  expect_true(is.numeric(ml$bank_POINT_M))
  expect_true(is.numeric(ml$DEM_Z))
  expect_true(is.numeric(ml$valley_POINT_X))
  expect_true(is.numeric(ml$valley_POINT_Y))
  expect_true(is.numeric(ml$valley_POINT_M))
  expect_true(is.numeric(ml$downstream_y))
  expect_true(is.numeric(ml$downstream_x))
  expect_true(is.numeric(ml$meander_length))
})

test_that("check output", {
  expect_equal(length(ml$loop),
               length(unique(na.omit(fluvgeo::sin_bankline_points_sf$loop))) )
})
