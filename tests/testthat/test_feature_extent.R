library(fluvgeo)
context("feature_extent")

# sf
flowline_fc      <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/flowline")
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_floodplain")
flowline_sf      <- fluvgeo::fc2sf(flowline_fc)
cross_section_sf <- fluvgeo::fc2sf(cross_section_fc)

fp_extent_sf <- feature_extent(fluvgeo::sin_flowline_points_sf)
rc_extent_sf <- feature_extent(fluvgeo::sin_riffle_channel_sf)


test_that("Check sf feature extents", {
  expect_true(class(fp_extent_sf)[1] == "Extent")
  expect_true(class(rc_extent_sf)[1] == "Extent")
})

