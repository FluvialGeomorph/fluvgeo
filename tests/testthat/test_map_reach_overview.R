library(fluvgeo)
library(tmap)
context("map_reach_overview")

flowline_fc      <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/flowline")
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_floodplain")
flowline_sf      <- fluvgeo::fc2sf(flowline_fc)
cross_section_sf <- fluvgeo::fc2sf(cross_section_fc)
xs_label_freq = 2
exaggeration = 1
extent_factor = 1.5

# Create the aerial map
map <- map_reach_overview(flowline_sf = flowline_sf,
                          cross_section_sf = cross_section_sf,
                          background = "none",
                          xs_label_freq = xs_label_freq,
                          extent_factor = extent_factor)

print(map)

# Create the aerial map
aerial_map <- map_reach_overview(flowline_sf = flowline_sf,
                              cross_section_sf = cross_section_sf,
                              background = "aerial",
                              xs_label_freq = xs_label_freq,
                              extent_factor = extent_factor)

print(aerial_map)

# Create the elevation map
elevation_map <- map_reach_overview(flowline_sf = flowline_sf,
                                    cross_section_sf = cross_section_sf,
                                    background = "elevation",
                                    xs_label_freq = xs_label_freq,
                                    exaggeration = exaggeration,
                                    extent_factor = extent_factor)

print(elevation_map)

test_that("check map_reach_overview aerial", {
  expect_true("tmap" %in% class(map))
  expect_error(print(map), NA)
})

test_that("check map_reach_overview aerial", {
  expect_true("tmap" %in% class(aerial_map))
  expect_error(print(aerial_map), NA)
})

test_that("check map_reach_overview elevation", {
  expect_true("tmap" %in% class(elevation_map))
  expect_error(print(elevation_map), NA)
})
