library(fgm)
context("slope_sinuosity")

# Extract attribute data from the fgm::sin_flowline_points SpatialPointsDataFrame
sin_flowline_points_df <- fgm::sin_flowline_points@data

# Extract data from the fgm::sin_riffle SpatialLinesDataFrame
sin_riffle_df <- fgm::sin_riffle@data

# Call the slope_sinuosity function for a flowline
sin_flowline_ss <- slope_sinuosity(sin_flowline_points_df,
                                   lead_lag = 1000)

# Call the slope_sinuosity function for a cross section
sin_riffle_ss <- slope_sinuosity(sin_riffle_df,
                                 lead_lag = 1,
                                 loess_span = 5)

test_that("Check output data structures", {
  assert_that(check_data_structure(sin_flowline_ss, "slope_sinuosity"),
              msg = "'sin_flowline_ss' does not meet the data specification")
  assert_that(check_data_structure(sin_riffle_ss, "slope_sinuosity"),
              msg = "'sin_riffle_ss' does not meet the data specification")
})

test_that("Check that water flows downhill", {
  assert_that(check_data_structure(sin_flowline_ss, "downhill"),
              msg = "'sin_flowline_ss' does not flow downhill")
})
