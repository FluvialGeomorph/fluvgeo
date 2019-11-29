library(fluvgeo)
context("check_loop_points")

loop_points <- fluvgeo::sin_loop_points_sp

# Make some bad data
## Loop 1 missing start point
lp_bad1 <- loop_points[loop_points@data$loop != 1 |
                         loop_points@data$position != "start", ]

## Loop 3 missing apex point
lp_bad2 <- loop_points[loop_points@data$loop != 3 |
                         loop_points@data$position != "apex", ]

## Loop 4 missing end point
lp_bad3 <- loop_points[loop_points@data$loop != 4 |
                         loop_points@data$position != "end", ]

test_that("check loop points", {
  expect_true(check_loop_points(fluvgeo::sin_loop_points_sp))
})

test_that("check for missing loop points", {
  expect_error(check_loop_points(lp_bad1))
  expect_error(check_loop_points(lp_bad2))
  expect_error(check_loop_points(lp_bad3))
})

test_that("check not loop points", {
  expect_error(check_loop_points(fluvgeo::sin_banklines_sp))
  expect_error(check_loop_points(fluvgeo::sin_features_sp))
  expect_error(check_loop_points(fluvgeo::sin_riffle_channel_points_sp))
})
