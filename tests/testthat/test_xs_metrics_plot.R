library(fluvgeo)
context("xs_metrics_plot")

# Extract data from the fluvgeo::sin_xs_dimensions SpatialPointsDataFrame
sin_xs_dims_df <- fluvgeo::sin_riffle_floodplain_dims_planform_sp@data

# Call the xs_metrics_plot function
sin_metrics <- xs_metrics_plot(reach_xs_dims = sin_xs_dims_df)


test_that("check xs metrics plot", {
  expect_true("ggplot" %in% class(sin_metrics))
  expect_error(print(sin_metrics), NA)
})


