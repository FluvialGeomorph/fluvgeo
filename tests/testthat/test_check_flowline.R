library(fluvgeo)
context("check_flowline")

# Create testing data
## fields at the `create_flowline` stage
sin_fl_1 <- fluvgeo::sin_flowline_sf[, c("OBJECTID", "ReachName")]

## fields at the `profile_points` stage
sin_fl_2 <- fluvgeo::sin_flowline_sf[, c("OBJECTID", "ReachName",
                                     "from_measure", "to_measure")]


test_that("check step `create_flowline`", {
  expect_true(check_flowline(sin_fl_1, "create_flowline"))
})

test_that("check step `profile_points`", {
  expect_true(check_flowline(sin_fl_2, "profile_points"))
})

test_that("other data sturctures", {
  expect_error(check_flowline(fluvgeo::sin_features_sf, "profile_points"))
  expect_error(check_flowline(fluvgeo::sin_banklines_sf, "profile_points"))
  expect_error(check_flowline(fluvgeo::sin_loop_points_sf, "profile_points"))
})

test_that("profile-ready flowlines reject renamed, mistyped and invalid measures", {
  renamed <- sin_fl_2
  names(renamed)[names(renamed) == "from_measure"] <- "fromMeasure"
  expect_error(check_flowline(renamed, "profile_points"), "from_measure")

  mistyped <- sin_fl_2
  mistyped$to_measure <- as.character(mistyped$to_measure)
  expect_error(check_flowline(mistyped, "profile_points"), "to_measure")

  reversed <- sin_fl_2
  reversed$to_measure <- reversed$from_measure
  expect_error(check_flowline(reversed, "profile_points"), "zero length")
})
