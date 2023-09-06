library(fluvgeo)
context("flowline_metrics")

# Extract the gradient and features sf
sin_gradient_sp <- fluvgeo::sin_gradient_sf
sin_features_sp <- fluvgeo::sin_features_sf

# Call the flowline_metrics plot function
sin_flowline_metrics <- flowline_metrics(gradient_sf = sin_gradient_sf,
                                         features_sf = sin_features_sf)

print(sin_flowline_metrics)

test_that("check xs metrics plot", {
  expect_true("ggplot" %in% class(sin_flowline_metrics))
  expect_error(print(sin_flowline_metrics), NA)
})
