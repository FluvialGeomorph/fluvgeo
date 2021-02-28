library(fluvgeo)
context("map_reach_metric")

# Define geomorphic metric
wdr <- new(Class = "FluvialGeomorphicMetric",
           metric = "Width Depth Ratio",
           definition = "bankfull width / bankfull depth",
           variable = "xs_width_depth_ratio",
           threshold_breaks = c(0, 10, 20, Inf),
           threshold_labels = c("Incised",
                                "Stable",
                                "Overwidened"),
           source = "Dunn & Leopold, 1978")

# Get feature class test data in sf format
flowline_fc     <- file.path(system.file("extdata", "y2016_R1.gdb",
                                         package = "fluvgeo"),
                             "flowline")
# Level 2
xs_dimensions_L2_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                             package = "fluvgeo"),
                                 "xs_50_dims_L2")
# Level 3
xs_dimensions_L3_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                             package = "fluvgeo"),
                                 "xs_50_dims_L3")

# Convert feature classes to an sf objects
flowline_sf         <- fluvgeo::fc2sf(flowline_fc)
xs_dimensions_L2_sf <- fluvgeo::fc2sf(xs_dimensions_L2_fc)
xs_dimensions_L3_sf <- fluvgeo::fc2sf(xs_dimensions_L3_fc)

xs_label_freq = 2
background = "aerial"
exaggeration = 20
extent_factor = 1.1

# Create the reach metric map using sf input, Level 2
wdr_map_L2_sf <- map_reach_metric(metric = wdr,
                                  flowline_sf = flowline_sf,
                                  xs_dimensions = xs_dimensions_L2_sf,
                                  xs_label_freq = xs_label_freq,
                                  background = background,
                                  exaggeration = exaggeration,
                                  extent_factor = extent_factor)
print(wdr_map_L2_sf)

# Create the reach metric map using sf input, Level 3
wdr_map_L3_sf <- map_reach_metric(metric = wdr,
                                  flowline_sf = flowline_sf,
                                  xs_dimensions = xs_dimensions_L3_sf,
                                  xs_label_freq = xs_label_freq,
                                  background = "elevation",
                                  exaggeration = exaggeration,
                                  extent_factor = extent_factor)
print(wdr_map_L3_sf)

# test_that("check map_reach_metric using sp input", {
#   expect_true("tmap" %in% class(wdr_map_sp))
#   expect_error(print(wdr_map_sp), NA)
# })

test_that("check map_reach_metric using sf input, Level 2", {
  expect_true("tmap" %in% class(wdr_map_L2_sf))
  expect_error(print(wdr_map_L2_sf), NA)
})

test_that("check map_reach_metric using sf input, Level 3", {
  expect_true("tmap" %in% class(wdr_map_L3_sf))
  expect_error(print(wdr_map_L3_sf), NA)
})
