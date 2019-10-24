library(fgm)
context("planform_dimensions")

# Calculate cross section planform dimensions
xs_dims_plan <- planform_dimensions(fgm::sin_riffle_floodplain_dims_sp,
                                    fgm::sin_bankline_points_sp)

test_that("check data structure", {
  expect_true(check_cross_section_dimensions(xs_dims_plan, "planform"))
})
