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

fp_extent_sf <- feature_extent(fluvgeo::sin_flowline_points_sp)
rc_extent_sf <- feature_extent(fluvgeo::sin_riffle_channel_sp)

# sp
fp_extent_sp <- feature_extent(fluvgeo::sin_flowline_points_sp)
rc_extent_sp <- feature_extent(fluvgeo::sin_riffle_channel_sp)

test_that("Check sf feature extents", {
  expect_true(class(fp_extent_sf)[1] == "Extent")
  expect_true(class(rc_extent_sf)[1] == "Extent")
})

test_that("Check sp feature extents", {
  expect_true(class(fp_extent_sp)[1] == "Extent")
  expect_true(class(rc_extent_sp)[1] == "Extent")
})
