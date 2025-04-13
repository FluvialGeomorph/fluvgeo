flowline_fc      <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeodata"),
                              "feature_dataset/flowline")
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeodata"),
                              "feature_dataset/riffle_floodplain")
flowline_sf      <- fluvgeo::fc2sf(flowline_fc, quiet = TRUE)
cross_section_sf <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)

feature = flowline_sf
extent_factor = 1

fl_extent <- map_extent(flowline_sf)
xs_extent <- map_extent(cross_section_sf)

test_that("output type", {
  expect_true(class(fl_extent)[1] == "bbox")
  expect_true(class(xs_extent)[1] == "bbox")
})
