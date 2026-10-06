library(fluvgeo)
context("check_flowline_points")

test_that("check flowline points", {
  expect_true(check_flowline_points(fluvgeo::sin_flowline_points_sf))
})

test_that("not flowline points", {
  expect_error(check_flowline_points(fluvgeo::sin_flowline_sf))
  expect_error(check_flowline_points(fluvgeo::sin_loop_points_sf))
  expect_error(check_flowline_points(fluvgeo::sin_banklines_sf))
})

test_that("exact legacy field names and types are enforced", {
  renamed <- fluvgeo::sin_flowline_points_sf
  names(renamed)[names(renamed) == "POINT_M"] <- "point_m"
  expect_error(check_flowline_points(renamed), "POINT_M")

  mistyped <- fluvgeo::sin_flowline_points_sf
  mistyped$Z <- as.character(mistyped$Z)
  expect_error(check_flowline_points(mistyped), "Numeric field 'Z'")
})

test_that("FG Studio replacement profile enforces kilometer compatibility", {
  points <- fluvgeo::sin_flowline_points_sf
  points$km_to_mouth <- points$POINT_M
  points$POINT_M_uncalibrated <- points$POINT_M
  points$calibration_diff <- 0
  points$POINT_M_units <- "km"
  points$POINT_M_uncalibrated_units <- "km"
  points$calibration_diff_units <- "km"
  points$km_to_mouth_units <- "km"
  expect_true(check_flowline_points(points, "fgstudio_replacement"))

  changed_units <- points
  changed_units$POINT_M <- changed_units$POINT_M * 1000
  changed_units$POINT_M_units <- "m"
  expect_error(check_flowline_points(changed_units, "fgstudio_replacement"),
               "identical kilometres|must be 'km'")

  changed_difference <- points
  changed_difference$calibration_diff[1] <- 1
  expect_error(check_flowline_points(changed_difference,
    "fgstudio_replacement"), "calibration_diff")
})

test_that("ordered measures tolerate floating-point equality at Reach boundaries", {
  points <- fluvgeo::sin_flowline_points_sf
  points$POINT_M[2] <- points$POINT_M[1] - .Machine$double.eps
  expect_true(check_flowline_points(points))
})
