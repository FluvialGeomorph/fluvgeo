library(fluvgeo)
context("xs_metric_plot")

# Create the fluvgeo::FluvialGeomorphicMetric object
wdr <- new(Class = "FluvialGeomorphicMetric",
           metric = "Width Depth Ratio",
           definition = "bankfull width / bankfull depth",
           variable = "xs_width_depth_ratio",
           threshold_breaks = c(0, 10, 20, Inf),
           threshold_labels = c("Incised",
                                "Stable",
                                "Overwidened"),
           source = "Dunn & Leopold, 1978")

# Get feature class test data
xs_planform_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeodata"),
                            "feature_dataset/riffle_floodplain_dims_L3")

# Convert feature classes to an sf objects
xs_planform_sf   <- fluvgeo::fc2sf(xs_planform_fc)

label_xs = TRUE
xs_label_freq = 2
profile_units = "miles"

metric_plot <- fluvgeo::xs_metric_plot(metric = wdr,
                                       reach_xs_dims = xs_planform_sf,
                                       label_xs = label_xs,
                                       xs_label_freq = xs_label_freq,
                                       profile_units = profile_units)
print(metric_plot)

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(metric_plot))
  expect_equal(metric_plot$labels$title,
               wdr@metric)
  expect_error(print(metric_plot), NA)
})
