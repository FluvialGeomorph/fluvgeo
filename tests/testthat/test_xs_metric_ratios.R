library(fgm)
context("xs_metric_ratios")

# Calculate cross section planform dimensions
xs_dims_plan <- planform_dimensions(fgm::sin_riffle_floodplain_dims_sp,
                                    fgm::sin_bankline_points_sp)

# Calculate cross section metric ratios
xs_dims_ratios <- xs_metric_ratios(xs_dims_plan)


test_that("check data structure", {
  expect_true(check_cross_section_dimensions(xs_dims_ratios, "metric_ratios"))
})

test_that("check values of rc_bfw_ratio < 10", {
  expect_equal(xs_dims_ratios$rc_bfw_ratio_10[4], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_10[5], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_10[6], 10)
  expect_equal(xs_dims_ratios$rc_bfw_ratio_10[10], 10)
})
