library(fluvgeo)
context("check_banklines")

# sf
# Get feature class test data in sf format
banklines_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                      package = "fluvgeo"),
                          "feature_dataset/banklines")
# Convert feature classes to an sf objects
banklines_sf <- fluvgeo::fc2sf(banklines_fc)


# sp
test_that("check banklines", {
  expect_true(check_banklines(fluvgeo::sin_banklines_sf))
})

test_that("not bankline points", {
  expect_error(check_banklines(fluvgeo::sin_flowline_sf))
  expect_error(check_banklines(fluvgeo::sin_loop_points_sf))
  expect_error(check_banklines(fluvgeo::sin_bankline_points_sf))
})

# sf
test_that("check banklines", {
  expect_true(check_banklines(banklines_sf))
})
