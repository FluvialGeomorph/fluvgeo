context("xs_plot")

# Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Call the xs_plot function
sin_4_plot <- xs_plot(xs_points = sin_xs_points_df,
                      stream = "Sinsinawa",
                      xs_number = 4,
                      bankfull_elevation = 103.5)

test_that("Check parameters", {
  expect_error(xs_regional_metrics(10, "Sinsinawa", 4, 103.5),
               info = "xs_points not a dataframe")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-2], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the Seq field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-3], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the POINT_X field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-4], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the POINT_Y field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-5], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the POINT_M field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-6], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the Watershed_Area_SqMile field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-7], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the km_to_mouth field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-8], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the DEM_Z field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-9], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the Detrend_DEM_Z field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-10], "Sinsinawa",
                                   4, 103.5),
               info = "xs_points is missing the ReachName field")
  expect_error(xs_regional_metrics(sin_xs_points_df, "",
                                   4, 103.5),
               info = "stream is an empty string")
  expect_error(xs_regional_metrics(sin_xs_points_df, c("stream1", "stream2"),
                                   4, 103.5),
               info = "stream is a vector of length > 1")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4.1, 103.5),
               info = "xs_number is not an integer")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   c(4, 5), 103.5),
               info = "xs_number is a vector of length > 1")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, "a"),
               info = "bankfull_elevation is not numeric")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, c(103.5, 103.6)),
               info = "bankfull_elevation is a vector of length > 1")
})

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(sin_4_plot))
  expect_equal(sin_4_plot$labels$title,
               "Cross Section  4")
  expect_equal(sin_4_plot$labels$x,
               "Station Distance (feet, from right descending bank)")
  expect_equal(sin_4_plot$labels$y,
               "Detrended Elevation (feet)")
})
