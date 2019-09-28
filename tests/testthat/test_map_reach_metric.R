library(fgm)
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

# Use the fgm::sin_flowline_sp SpatialLinesDataFrame
sin_flowline_sp <- fgm::sin_flowline_sp

# Use the fgm::sin_xs_dimensions_sp SpatialLinesDataFrame
sin_xs_dimensions_sp <- fgm::sin_xs_dimensions_sp

wdr_map <- map_reach_metric(wdr, sin_flowline_sp, sin_xs_dimensions_sp)

test_that("check map_reach_metric", {
  expect_true("tmap" %in% class(wdr_map))
  expect_error(print(wdr_map), NA)
})
