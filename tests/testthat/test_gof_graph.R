library(fluvgeo)
context("gof_graph")

# Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fluvgeo::sin_riffle_channel_points_sp@data

# Set variable values
streams <- "Sinsinawa"
regions <- c("Eastern United States", "IN Central Till Plain")
bankfull_elevations <- seq(103, 104, 0.1)

# Call the xs_dimensions function
sin <- xs_dimensions(xs_points = sin_xs_points_df,
                     streams = streams,
                     regions = regions,
                     bankfull_elevations = bankfull_elevations)

# Call the build_gof_stats function
sin_gof <- build_gof_stats(xs_dims = sin,
                           streams = streams,
                           regions = regions,
                           bankfull_elevations = bankfull_elevations)

# Call the gof_graph function
sin_gof_graph <- gof_graph(gof_stats = sin_gof,
                           stream = streams,
                           bankfull_elevation = 103.5,
                           stat = "MAE")

test_that("Check parameters", {
  expect_error(gof_graph(10, streams, 103.5, "MAE"),
               info = "gof_stats not a data frame")
  expect_error(gof_graph(sin_gof[,-1], streams, 103.5, "MAE"),
               info = "gof_stats is missing reach_name field")
  expect_error(gof_graph(sin_gof[,-2], streams, 103.5, "MAE"),
               info = "gof_stats is missing region field")
  expect_error(gof_graph(sin_gof[,-3], streams, 103.5, "MAE"),
               info = "gof_stats is missing bankfull_elevation field")
  expect_error(gof_graph(sin_gof[,-4], streams, 103.5, "MAE"),
               info = "gof_stats is missing rmse_area field")
  expect_error(gof_graph(sin_gof[,-5], streams, 103.5, "MAE"),
               info = "gof_stats is missing rmse_width field")
  expect_error(gof_graph(sin_gof[,-6], streams, 103.5, "MAE"),
               info = "gof_stats is missing rmse_depth field")
  expect_error(gof_graph(sin_gof[,-7], streams, 103.5, "MAE"),
               info = "gof_stats is missing mae_area field")
  expect_error(gof_graph(sin_gof[,-8], streams, 103.5, "MAE"),
               info = "gof_stats is missing mae_width field")
  expect_error(gof_graph(sin_gof[,-9], streams, 103.5, "MAE"),
               info = "gof_stats is missing mae_depth field")
  expect_error(gof_graph(sin_gof, 8, 103.5, "MAE"),
               info = "streams is not a character vector")
  expect_error(gof_graph(sin_gof, streams, "a", "MAE"),
               info = "bankfull_elevation is not a numeric vector")
  expect_error(gof_graph(sin_gof, streams, 103.5, 4),
               info = "stat is not a character vector")
  expect_error(gof_graph(sin_gof, streams, 103.5, "xxx"),
               info = "stat is not a character vector of 'MAE' or 'RMSE'")
})

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(sin_gof_graph))
  expect_equal(sin_gof_graph$labels$x,
               "Detrended Bankfull Elevation (feet)")
  expect_equal(sin_gof_graph$labels$y,
               "MAE")
})

test_that("Check the 'stat' parameter", {
  expect_equal(gof_graph(gof_stats = sin_gof,
                         stream = streams,
                         bankfull_elevation = 103.5,
                         stat = "MAE")$labels$y,
               "MAE")
  expect_equal(gof_graph(gof_stats = sin_gof,
                         stream = streams,
                         bankfull_elevation = 103.5,
                         stat = "RMSE")$labels$y,
               "RMSE")
})
