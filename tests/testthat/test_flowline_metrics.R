library(fluvgeo)
context("flowline_metrics")

# Extract the gradient and features SpatialPointsDataFrames
sin_gradient_sp <- fluvgeo::sin_gradient_sp
sin_features_sp <- fluvgeo::sin_features_sp

# Call the flowline_metrics plot function
sin_flowline_metrics <- flowline_metrics(gradient_sp = sin_gradient_sp,
                                         features_sp = sin_features_sp)

print(sin_flowline_metrics)

test_that("check xs metrics plot", {
  expect_true("ggplot" %in% class(sin_flowline_metrics))
  expect_error(print(sin_flowline_metrics), NA)
})
