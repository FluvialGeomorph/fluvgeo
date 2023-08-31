library(fluvgeo)
context("check_cross_section_points")

test_that("check cross section points", {
  expect_true(check_cross_section_points(fluvgeo::sin_riffle_channel_points_sf,
                                         "station_points"))
})

test_that("not cross section points", {
  expect_error(check_cross_section_points(fluvgeo::sin_flowline_sf,
                                          "station_points"))
  expect_error(check_cross_section_points(fluvgeo::sin_loop_points_sf,
                                          "station_points"))
  expect_error(check_cross_section_points(fluvgeo::sin_banklines_sf,
                                          "station_points"))
})
