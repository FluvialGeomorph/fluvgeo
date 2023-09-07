library(fluvgeo)
context("check_loop_points")

loop_points <- fluvgeo::sin_loop_points_sf

# Make some bad data
## Loop 1 missing start point
lp_bad1 <- loop_points[loop_points$loop != 1 |
                         loop_points$position != "start", ]

## Loop 3 missing apex point
lp_bad2 <- loop_points[loop_points$loop != 3 |
                         loop_points$position != "apex", ]

## Loop 4 missing end point
lp_bad3 <- loop_points[loop_points$loop != 4 |
                         loop_points$position != "end", ]

test_that("check loop points", {
  expect_true(check_loop_points(fluvgeo::sin_loop_points_sf))
})

test_that("check for missing loop points", {
  expect_error(check_loop_points(lp_bad1))
  expect_error(check_loop_points(lp_bad2))
  expect_error(check_loop_points(lp_bad3))
})

test_that("check not loop points", {
  expect_error(check_loop_points(fluvgeo::sin_banklines_sf))
  expect_error(check_loop_points(fluvgeo::sin_features_sf))
  expect_error(check_loop_points(fluvgeo::sin_riffle_channel_points_sf))
})
