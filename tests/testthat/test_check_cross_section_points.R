library(fgm)
context("check_cross_section_points")

test_that("check cross section points", {
  expect_true(check_cross_section_points(fgm::sin_riffle_channel_points_sp,
                                         "station_points"))
})

test_that("not cross section points", {
  expect_error(check_cross_section_points(fgm::sin_flowline_sp,
                                          "station_points"))
  expect_error(check_cross_section_points(fgm::sin_loop_points_sp,
                                          "station_points"))
  expect_error(check_cross_section_points(fgm::sin_banklines_sp,
                                          "station_points"))
})
