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

test_that("Check", {})
