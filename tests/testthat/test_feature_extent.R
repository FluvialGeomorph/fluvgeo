library(fluvgeo)
context("feature_extent")

flowline_fc      <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/flowline")
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_floodplain")
flowline_sf      <- fluvgeo::fc2sf(flowline_fc)
cross_section_sf <- fluvgeo::fc2sf(cross_section_fc)

fl_extent <- feature_extent(flowline_sf)
xs_extent <- feature_extent(cross_section_sf)

test_that("Check sf feature extents", {
  expect_true(class(fl_extent)[1] == "Extent")
  expect_true(class(xs_extent)[1] == "Extent")
})

