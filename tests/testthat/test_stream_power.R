library(fluvgeo)
context("stream_power")

# Calculate cross section dimensions
xs_dims <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
                                    xs_points = fluvgeo::sin_riffle_channel_points_sp,
                                    bankfull_elevation = 103,
                                    lead_n = 1,
                                    use_smoothing = TRUE,
                                    loess_span = 0.5,
                                    vert_units = "ft")
# Calculate shear stress
xs_dims_ss <- shear_stress(xs_dims)

# Calculate stream power, method: regional curve
xs_dims_sp <- stream_power(xs_dims_ss,
                           discharge_method = "regional_curve",
                           region = "Illinois River",
                           drainage_area = 41)

# Calculate stream power, method: model_measure
xs_dims_sp2 <- stream_power(xs_dims_ss,
                            discharge_method = "model_measure",
                            discharge_value = 1200)

test_that("check fields exist by name", {
  expect_true("Seq"                   %in% colnames(xs_dims_sp))
  expect_true("Z_smooth"              %in% colnames(xs_dims_sp))
  expect_true("upstream_x"            %in% colnames(xs_dims_sp))
  expect_true("upstream_y"            %in% colnames(xs_dims_sp))
  expect_true("downstream_x"          %in% colnames(xs_dims_sp))
  expect_true("downstream_y"          %in% colnames(xs_dims_sp))
  expect_true("upstream_z"            %in% colnames(xs_dims_sp))
  expect_true("downstream_z"          %in% colnames(xs_dims_sp))
  expect_true("upstream_m"            %in% colnames(xs_dims_sp))
  expect_true("downstream_m"          %in% colnames(xs_dims_sp))
  expect_true("rise"                  %in% colnames(xs_dims_sp))
  expect_true("run"                   %in% colnames(xs_dims_sp))
  expect_true("stream_length"         %in% colnames(xs_dims_sp))
  expect_true("valley_length"         %in% colnames(xs_dims_sp))
  expect_true("sinuosity"             %in% colnames(xs_dims_sp))
  expect_true("slope"                 %in% colnames(xs_dims_sp))
  expect_true("bankfull_elevation"    %in% colnames(xs_dims_sp))
  expect_true("drainage_area"         %in% colnames(xs_dims_sp))
  expect_true("xs_area"               %in% colnames(xs_dims_sp))
  expect_true("xs_width"              %in% colnames(xs_dims_sp))
  expect_true("xs_depth"              %in% colnames(xs_dims_sp))
  expect_true("discharge"             %in% colnames(xs_dims_sp))
  expect_true("fp_area"               %in% colnames(xs_dims_sp))
  expect_true("fp_width"              %in% colnames(xs_dims_sp))
  expect_true("fp_depth"              %in% colnames(xs_dims_sp))
  expect_true("xs_width_depth_ratio"  %in% colnames(xs_dims_sp))
  expect_true("xs_entrenchment_ratio" %in% colnames(xs_dims_sp))
  expect_true("watersurface_elev"     %in% colnames(xs_dims_sp))
  expect_true("bankfull_elev"         %in% colnames(xs_dims_sp))
  expect_true("floodprone_elev"       %in% colnames(xs_dims_sp))
  expect_true("shear_stress"          %in% colnames(xs_dims_sp))
  expect_true("stream_power"          %in% colnames(xs_dims_sp))
  expect_true("stream_power_lane"     %in% colnames(xs_dims_sp))
  expect_true("unit_stream_power"     %in% colnames(xs_dims_sp))
})

test_that("check field data type", {
  expect_true(is.numeric(xs_dims_sp$Seq))
  expect_true(is.numeric(xs_dims_sp$Z_smooth))
  expect_true(is.numeric(xs_dims_sp$upstream_x))
  expect_true(is.numeric(xs_dims_sp$upstream_y))
  expect_true(is.numeric(xs_dims_sp$downstream_x))
  expect_true(is.numeric(xs_dims_sp$downstream_y))
  expect_true(is.numeric(xs_dims_sp$upstream_z))
  expect_true(is.numeric(xs_dims_sp$downstream_z))
  expect_true(is.numeric(xs_dims_sp$upstream_m))
  expect_true(is.numeric(xs_dims_sp$downstream_m))
  expect_true(is.numeric(xs_dims_sp$rise))
  expect_true(is.numeric(xs_dims_sp$run))
  expect_true(is.numeric(xs_dims_sp$stream_length))
  expect_true(is.numeric(xs_dims_sp$valley_length))
  expect_true(is.numeric(xs_dims_sp$sinuosity))
  expect_true(is.numeric(xs_dims_sp$slope))
  expect_true(is.numeric(xs_dims_sp$bankfull_elevation))
  expect_true(is.numeric(xs_dims_sp$drainage_area))
  expect_true(is.numeric(xs_dims_sp$xs_area))
  expect_true(is.numeric(xs_dims_sp$xs_width))
  expect_true(is.numeric(xs_dims_sp$xs_depth))
  expect_true(is.numeric(xs_dims_sp$discharge))
  expect_true(is.numeric(xs_dims_sp$fp_area))
  expect_true(is.numeric(xs_dims_sp$fp_width))
  expect_true(is.numeric(xs_dims_sp$fp_depth))
  expect_true(is.numeric(xs_dims_sp$xs_width_depth_ratio))
  expect_true(is.numeric(xs_dims_sp$xs_entrenchment_ratio))
  expect_true(is.numeric(xs_dims_sp$watersurface_elev))
  expect_true(is.numeric(xs_dims_sp$bankfull_elev))
  expect_true(is.numeric(xs_dims_sp$floodprone_elev))
  expect_true(is.numeric(xs_dims_sp$shear_stress))
  expect_true(is.numeric(xs_dims_sp$stream_power))
  expect_true(is.numeric(xs_dims_sp$stream_power_lane))
  expect_true(is.numeric(xs_dims_sp$unit_stream_power))
})

test_that("check required parameters", {
  expect_error(stream_power(xs_dims, discharge_method = "model_measure"))
  expect_error(stream_power(xs_dims, discharge_method = "regional_curve"))
  expect_error(stream_power(xs_dims, discharge_method = "regional_curve",
                            region = "Illinois River"))
  expect_error(stream_power(xs_dims, discharge_method = "width_relationship"))
})

test_that("check data structure", {
  expect_true(check_cross_section_dimensions(xs_dims_sp, "stream_power"))
  expect_true(check_cross_section_dimensions(xs_dims_sp2, "stream_power"))
})
