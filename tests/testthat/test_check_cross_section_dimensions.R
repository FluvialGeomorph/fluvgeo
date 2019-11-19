library(fluvgeo)
context("check_cross_section_dimensions")

# Create testing data
## Step: cross_section_dimensions
xs_dims <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
                                    xs_points = fluvgeo::sin_riffle_channel_points_sp,
                                    bankfull_elevation = 103,
                                    lead_n = 1,
                                    use_smoothing = TRUE,
                                    loess_span = 0.5,
                                    vert_units = "ft")
## Step: shear stress
xs_dims_ss <- shear_stress(xs_dims)

## Step: stream_power
xs_dims_sp <- stream_power(xs_dims_ss,
                           discharge_method = "regional_curve",
                           region = "Illinois River",
                           drainage_area = 41)

## Step: planform
xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_sp,
                                    fluvgeo::sin_bankline_points_sp)

## Step: metric_ratios
xs_dims_ratios <- xs_metric_ratios(xs_dims_plan)


test_that("check cross_section_dimension step", {
  expect_true(check_cross_section_dimensions(xs_dims, "cross_section_dimensions"))
})

test_that("check shear_stress step", {
  expect_true(check_cross_section_dimensions(xs_dims_ss, "shear_stress"))
})

test_that("check stream_power step", {
  expect_true(check_cross_section_dimensions(xs_dims_sp, "stream_power"))
})

test_that("check planform step", {
  expect_true(check_cross_section_dimensions(xs_dims_plan, "planform"))
})

test_that("check mertic_ratio step", {
  expect_true(check_cross_section_dimensions(xs_dims_ratios, "metric_ratio"))
})
