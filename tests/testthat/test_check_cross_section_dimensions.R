library(fluvgeo)
context("check_cross_section_dimensions")

# Create testing data
## Step: cross_section_dimensions
xs_dims <- cross_section_dimensions_L2(xs = fluvgeo::sin_riffle_channel_sf,
                                    xs_points = fluvgeo::sin_riffle_channel_points_sf,
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
xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_L2_sf,
                                    fluvgeo::sin_bankline_points_sf)

## Step: metric_ratios
xs_dims_ratios <- xs_metric_ratios(xs_dims_plan)


# sf
xs_dims_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                    package = "fluvgeodata"),
                        "feature_dataset/riffle_floodplain_dims_L2")
xs_dims_sf <- fluvgeo::fc2sf(xs_dims_fc)


test_that("check cross_section_dimension step", {
  expect_true(check_cross_section_dimensions(xs_dims, "level_1"))
})

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

test_that("check mertic_ratios step", {
  expect_true(check_cross_section_dimensions(xs_dims_ratios, "metric_ratios"))
})


# sf
test_that("check stream_power step, sf", {
  expect_true(check_cross_section_dimensions(xs_dims_sf, "stream_power"))
})


test_that("check for wrong step", {
  expect_error(check_cross_section_dimensions(xs_dims_ratios, "metric_ratio"))
})
