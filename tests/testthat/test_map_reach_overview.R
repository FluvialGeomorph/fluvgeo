library(fluvgeo)
context("map_reach_overview")

# Use the fluvgeo::sin_flowline_sp SpatialLinesDataFrame
sin_flowline_sp <- fluvgeo::sin_flowline_sp

# Use the fluvgeo::sin_riffle_floodplain_sp SpatialLinesDataFrame
sin_riffle_channel_sp <- fluvgeo::sin_riffle_channel_sp

# Create the map
sin_map <- map_reach_overview(sin_flowline_sp, sin_riffle_channel_sp)

print(sin_map)

test_that("check map_reach_overview", {
  expect_true("tmap" %in% class(sin_map))
  expect_error(print(sin_map), NA)
})
