library(fluvgeo)
context("check_flowline")

# Create testing data
## fields at the `create_flowline` stage
sin_fl_1 <- fluvgeo::sin_flowline_sp[, c("OBJECTID", "ReachName")]

## fields at the `profile_points` stage
sin_fl_2 <- fluvgeo::sin_flowline_sp[, c("OBJECTID", "ReachName",
                                     "from_measure", "to_measure")]


test_that("check step `create_flowline`", {
  expect_true(check_flowline(sin_fl_1, "create_flowline"))
})

test_that("check step `profile_points`", {
  expect_true(check_flowline(sin_fl_2, "profile_points"))
})

test_that("other data sturctures", {
  expect_error(check_flowline(fluvgeo::sin_features_sp, "profile_points"))
  expect_error(check_flowline(fluvgeo::sin_banklines_sp, "profile_points"))
  expect_error(check_flowline(fluvgeo::sin_loop_points_sp, "profile_points"))
})
