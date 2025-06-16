library(fluvgeo)
context("xs_plot")

# Extract attribute data from the fluvgeo::sin_riffle_channel_points_sf
# sf object
sin_xs_points_df <- fluvgeo::sin_riffle_channel_points_sf

# Call the xs_plot function
sin_4_plot <- fluvgeo::xs_plot(xs_points = sin_xs_points_df,
                               stream = "Sinsinawa",
                               xs_number = 4,
                               bankfull_elevation = 103.5)


test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(sin_4_plot))
  expect_equal(sin_4_plot$labels$title,
               "Cross Section 4")
  expect_equal(sin_4_plot$labels$x,
               "Station Distance (feet, from right descending bank)")
  expect_equal(sin_4_plot$labels$y,
               "Relative Elevation (feet)")
})
