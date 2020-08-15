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

# sp
# Get feature class test data in sp format
flowline_sp      <- fluvgeo::sin_flowline_sp
xs_dimensions_sp <- fluvgeo::sin_riffle_floodplain_dims_planform_sp

# Create the reach metric map
wdr_map_sp <- map_reach_metric(wdr,
                               flowline = flowline_sp,
                               xs_dimensions = xs_dimensions_sp)

# sf
# Get feature class test data in sf format
flowline_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                        package = "fluvgeo"),
                            "flowline")
xs_dimensions_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                     package = "fluvgeo"),
                             "riffle_floodplain_dims_planform")

# Convert feature classes to an sf objects
flowline_sf      <- fluvgeo::fc2sf(flowline_fc)
xs_dimensions_sf <- fluvgeo::fc2sf(xs_dimensions_fc)

# Create the reach metric map using sp input
wdr_map_sp <- map_reach_metric(wdr,
                               flowline = flowline_sp,
                               xs_dimensions = xs_dimensions_sp)
print(wdr_map_sp)

# Create the reach metric map using sf input
wdr_map_sf <- map_reach_metric(wdr,
                               flowline = flowline_sf,
                               xs_dimensions = xs_dimensions_sf)
print(wdr_map_sf)

test_that("check map_reach_metric using sp input", {
  expect_true("tmap" %in% class(wdr_map_sp))
  expect_error(print(wdr_map_sp), NA)
})

test_that("check map_reach_metric using sf input", {
  expect_true("tmap" %in% class(wdr_map_sf))
  expect_error(print(wdr_map_sf), NA)
})
