library(fluvgeo)
context("xs_metrics_plot_L2")

# Get feature class test data
features_fc   <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "features")
xs_dims_fc    <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "riffle_floodplain_dims")

# Convert feature classes to an sf objects
xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
features_sf  <- fluvgeo::fc2sf(features_fc)

label_xs = TRUE
xs_label_freq = 2
profile_units = "miles"

# Call the xs_metrics_plot function
level_2_metrics <- xs_metrics_plot_L2(xs_dims_sf = xs_dims_sf,
                                      features_sf = features_sf,
                                      label_xs = label_xs,
                                      xs_label_freq = xs_label_freq,
                                      profile_units = profile_units)
print(level_2_metrics)

test_that("check xs metrics plot", {
  expect_true("ggplot" %in% class(level_2_metrics))
  expect_error(print(level_2_metrics), NA)
})

