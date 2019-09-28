library(fgm)
context("slope_sinuosity")

# Call the slope_sinuosity function for a flowline
sin_flowline_ss <- slope_sinuosity(fgm::sin_flowline_points_sp,
                                   lead_n = 1000, lag_n = 0)

# Call the slope_sinuosity function for a cross section
sin_riffle_channel_ss <- slope_sinuosity(fgm::sin_riffle_channel_sp,
                                         lead_n = 1, lag_n = 0,
                                         loess_span = 5)

test_that("Check output data structures", {
  expect_true(check_data_structure(sin_flowline_ss, "slope_sinuosity"))
  expect_true(check_data_structure(sin_riffle_channel_ss, "slope_sinuosity"))
})
