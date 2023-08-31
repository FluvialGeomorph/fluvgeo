library(fluvgeo)
context("xs_metrics")

# Extract attribute data from the fluvgeo::sin_riffle_floodplain_points_sf
# sf object
sin_xs_points_df <- fluvgeo::sin_riffle_floodplain_points_sf

# Call the xs_metrics function
sin_4 <- xs_metrics(xs_points = sin_xs_points_df,
                    stream = "Sinsinawa",
                    xs_number = 4,
                    bankfull_elevation = 103.5)

test_that("Check parameters", {
  expect_error(xs_metrics(xs_points = 10,
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-2],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-3],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-4],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-5],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-6],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-7],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-8],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-9],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df[,-10],
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = "",
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = c("stream1", "stream2"),
                          xs_number = 4,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = "Sinsinawa",
                          xs_number = 4.1,
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = "Sinsinawa",
                          xs_number = c(4, 5),
                          bankfull_elevation = 103.5))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = "a"))
  expect_error(xs_metrics(xs_points = sin_xs_points_df,
                          stream = "Sinsinawa",
                          xs_number = 4,
                          bankfull_elevation = c(103.5, 103.6)))
})

test_that("Check that fields exist by name", {
  expect_true("reach_name"            %in% colnames(sin_4))
  expect_true("cross_section"         %in% colnames(sin_4))
  expect_true("xs_type"               %in% colnames(sin_4))
  expect_true("bankfull_elevation"    %in% colnames(sin_4))
  expect_true("drainage_area"         %in% colnames(sin_4))
  expect_true("xs_area"               %in% colnames(sin_4))
  expect_true("xs_width"              %in% colnames(sin_4))
  expect_true("xs_depth"              %in% colnames(sin_4))
  expect_true("discharge"             %in% colnames(sin_4))
  expect_true("fp_area"               %in% colnames(sin_4))
  expect_true("fp_width"              %in% colnames(sin_4))
  expect_true("fp_depth"              %in% colnames(sin_4))
  expect_true("xs_width_depth_ratio"          %in% colnames(sin_4))
  expect_true("xs_width_depth_ratio_gte_one"  %in% colnames(sin_4))
  expect_true("xs_entrenchment_ratio"         %in% colnames(sin_4))
  expect_true("xs_entrenchment_ratio_gte_one" %in% colnames(sin_4))
  expect_true("watersurface_elev"     %in% colnames(sin_4))
  expect_true("bankfull_elev"         %in% colnames(sin_4))
  expect_true("floodprone_elev"       %in% colnames(sin_4))
})

test_that("Check that fields are of the correct data type", {
  expect_true(is.character(sin_4$reach_name))
  expect_true(is.numeric(sin_4$cross_section))
  expect_true(is.character(sin_4$xs_type))
  expect_true(is.numeric(sin_4$bankfull_elevation))
  expect_true(is.numeric(sin_4$drainage_area))
  expect_true(is.numeric(sin_4$xs_area))
  expect_true(is.numeric(sin_4$xs_width))
  expect_true(is.numeric(sin_4$xs_depth))
  expect_true(is.numeric(sin_4$discharge))
  expect_true(is.numeric(sin_4$fp_area))
  expect_true(is.numeric(sin_4$fp_width))
  expect_true(is.numeric(sin_4$fp_depth))
  expect_true(is.numeric(sin_4$xs_width_depth_ratio))
  expect_true(is.numeric(sin_4$xs_width_depth_ratio_gte_one))
  expect_true(is.numeric(sin_4$xs_entrenchment_ratio))
  expect_true(is.numeric(sin_4$xs_entrenchment_ratio_gte_one))
  expect_true(is.numeric(sin_4$watersurface_elev))
  expect_true(is.numeric(sin_4$bankfull_elev))
  expect_true(is.numeric(sin_4$floodprone_elev))
})

test_that("Check dimensions from known stream", {
  expect_match(sin_4$reach_name,            "Sinsinawa")
  expect_equal(sin_4$cross_section,         4,        tolerance = 1e-2)
  expect_match(sin_4$xs_type,               "DEM derived cross section")
  expect_equal(sin_4$bankfull_elevation,    103.5,    tolerance = 1e-2)
  expect_equal(sin_4$drainage_area,         40.27, tolerance = 1e-2)
  expect_equal(sin_4$xs_area,               323.2, tolerance = 1e-2)
  expect_equal(sin_4$xs_width,              129.2, tolerance = 1e-2)
  expect_equal(sin_4$xs_depth,              4.17, tolerance = 1e-2)
  expect_equal(sin_4$discharge,             0.0, tolerance = 1e-2)
  expect_equal(sin_4$fp_area,               983.9, tolerance = 1e-2)
  expect_equal(sin_4$fp_width,              486.2, tolerance = 1e-2)
  expect_equal(sin_4$fp_depth,              7.67, tolerance = 1e-2)
  # expect_equal(sin_4$xs_width_depth_ratio,  30.98, tolerance = 1e-2)
  expect_equal(sin_4$xs_entrenchment_ratio, 3.76, tolerance = 1e-2)
  expect_equal(sin_4$watersurface_elev,     641.51, tolerance = 1e-2)
  expect_equal(sin_4$bankfull_elev,         645.68, tolerance = 1e-2)
  expect_equal(sin_4$floodprone_elev,       649.18, tolerance = 1e-2)
})

