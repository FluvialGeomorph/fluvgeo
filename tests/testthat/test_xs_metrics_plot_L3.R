library(fluvgeo)
context("xs_metrics_plot_L3")

# Get feature class test data
features_fc   <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeodata"),
                           "feature_dataset/features")
xs_dims_fc    <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeodata"),
                           "feature_dataset/xs_50_dims_L3")

# Convert feature classes to an sf objects
xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
features_sf  <- fluvgeo::fc2sf(features_fc)

label_xs = TRUE
xs_label_freq = 10
profile_units = "feet"

# Call the xs_metrics_plot function
level_3_metrics <- xs_metrics_plot_L3(xs_dims_sf = xs_dims_sf,
                                      features_sf = features_sf,
                                      label_xs = label_xs,
                                      xs_label_freq = xs_label_freq,
                                      profile_units = profile_units)
print(level_3_metrics)

test_that("check xs_metrics_plot_L3", {
  expect_true("ggplot" %in% class(level_3_metrics))
  expect_error(print(level_3_metrics), NA)
})


