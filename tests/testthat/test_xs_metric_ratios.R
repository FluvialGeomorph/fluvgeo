library(fluvgeo)
context("xs_metric_ratios")

# Calculate cross section planform dimensions
xs_dims_L2 <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_L2_sf,
                                  fluvgeo::sin_bankline_points_sf)

# Calculate cross section metric ratios
xs_dims_ratios <- xs_metric_ratios(xs_dimensions = xs_dims_L2)


test_that("check data structure", {
  expect_true(check_cross_section_dimensions(xs_dims_ratios,
                                             "metric_ratios"))
})

test_that("check values of rc_bfw_ratio < 10", {
  expect_equal(xs_dims_ratios$rc_bfw_ratio_lte_10[4], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_lte_10[5], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_lte_10[6], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_lte_10[10], 10)
})
