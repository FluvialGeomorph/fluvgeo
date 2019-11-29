library(fluvgeo)
context("check_bankline_points")

bankline_points <- fluvgeo::sin_bankline_points_sp

# Create some bad data
## Simulate the loop = 3, bend = 2, position = start point missing
bad1 <- bankline_points
#bad1_df <- bad1@data
bad1@data[!is.na(bad1$loop) &
          (bad1$loop == 3 & bad1$bend == 2 &
          !(bad1$position %in% c("apex", "end"))), ]$position <- NA
bad1@data[!is.na(bad1$loop) &
            (bad1$loop == 3 & bad1$bend == 2 &
               !(bad1$position %in% c("apex", "end"))), ]$bend <- NA
bad1@data[!is.na(bad1$loop) &
            (bad1$loop == 3 & bad1$bend == 2 &
             bad1$position %in% "apex"), ]$bend <- 0
bad1@data[!is.na(bad1$loop) &
            (bad1$loop == 3 & is.na(bad1$bend)), ]$loop <- NA
#bad1_df <- bad1@data


test_that("check bankline points", {
  expect_true(check_bankline_points(bankline_points))
})

test_that("check for malformed bankline_points", {
  expect_error(check_bankline_points(bad1))
})

test_that("not bankline points", {
  expect_error(check_bankline_points(fluvgeo::sin_flowline_sp))
  expect_error(check_bankline_points(fluvgeo::sin_loop_points_sp))
  expect_error(check_bankline_points(fluvgeo::sin_banklines_sp))
})
