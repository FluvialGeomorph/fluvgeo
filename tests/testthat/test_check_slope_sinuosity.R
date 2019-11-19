library(fluvgeo)
context("check_slope_sinuosity")


# Call the slope_sinuosity function for a cross section
sin_riffle_channel_ss <- slope_sinuosity(fluvgeo::sin_riffle_channel_sp,
                                         lead_n = 1, lag_n = 0,
                                         loess_span = 5,
                                         vert_units = "ft")

test_that("check slope_sinuosity ", {
  expect_true(check_slope_sinuosity(sin_riffle_channel_ss))
})


