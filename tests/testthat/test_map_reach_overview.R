library(fluvgeo)
library(tmap)
context("map_reach_overview")

flowline_fc      <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "flowline")
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "riffle_floodplain")
flowline_sf      <- fluvgeo::fc2sf(flowline_fc)
cross_section_sf <- fluvgeo::fc2sf(cross_section_fc)

# Create the aerial map
aerial_map <- map_reach_overview(flowline_sf = flowline_sf,
                              cross_section_sf = cross_section_sf,
                              background = "aerial",
                              xs_label_freq = 10)

print(aerial_map)

# Create the elevation map
elevation_map <- map_reach_overview(flowline_sf = flowline_sf,
                                    cross_section_sf = cross_section_sf,
                                    background = "elevation",
                                    exaggeration = 50)

print(elevation_map)


test_that("check map_reach_overview aerial", {
  expect_true("tmap" %in% class(aerial_map))
  expect_error(print(aerial_map), NA)
})

test_that("check map_reach_overview elevation", {
  expect_true("tmap" %in% class(elevation_map))
  expect_error(print(elevation_map), NA)
})
