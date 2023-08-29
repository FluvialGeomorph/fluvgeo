library(fluvgeo)
context("planform_dimensions")

xs_dimensions <- sin_riffle_floodplain_dims_L2_sf
bankline_points <- sin_bankline_points_sf

# Calculate cross section planform dimensions
xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_L2_sf,
                                    fluvgeo::sin_bankline_points_sf)

test_that("check data structure", {
  expect_true(check_cross_section_dimensions(xs_dims_plan, "planform"))
})
