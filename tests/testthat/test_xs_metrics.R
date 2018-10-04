context("xs_metrics")

## Prepare the test data for known site
# Extract the attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Call the xs_metrics function
sin_4 <- xs_metrics(xs_points = sin_xs_points_df,
                    stream = "Sinsinawa",
                    xs_number = 4,
                    bankfull_elevation = 103.5)

test_that("Check that fields exist by name", {
  expect_true("reach_name"            %in% colnames(sin_4))
  expect_true("cross_section"         %in% colnames(sin_4))
  expect_true("xs_type"               %in% colnames(sin_4))
  expect_true("bankfull_elevation"    %in% colnames(sin_4))
  expect_true("drainage_area"         %in% colnames(sin_4))
  expect_true("xs_area"               %in% colnames(sin_4))
  expect_true("xs_width"              %in% colnames(sin_4))
  expect_true("xs_depth"              %in% colnames(sin_4))
  expect_true("fp_area"               %in% colnames(sin_4))
  expect_true("fp_width"              %in% colnames(sin_4))
  expect_true("fp_depth"              %in% colnames(sin_4))
  expect_true("xs_width_depth_ratio"  %in% colnames(sin_4))
  expect_true("xs_entrenchment_ratio" %in% colnames(sin_4))
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
  expect_true(is.numeric(sin_4$fp_area))
  expect_true(is.numeric(sin_4$fp_width))
  expect_true(is.numeric(sin_4$fp_depth))
  expect_true(is.numeric(sin_4$xs_width_depth_ratio))
  expect_true(is.numeric(sin_4$xs_entrenchment_ratio))
  expect_true(is.numeric(sin_4$watersurface_elev))
  expect_true(is.numeric(sin_4$bankfull_elev))
  expect_true(is.numeric(sin_4$floodprone_elev))
})

test_that("Check dimensions from known stream", {
  expect_match(sin_4$reach_name,            "Sinsinawa")
  expect_equal(sin_4$cross_section,         4,        tolerance = 1e-2)
  expect_match(sin_4$xs_type,               "DEM derived cross section")
  expect_equal(sin_4$bankfull_elevation,    103.5,    tolerance = 1e-2)
  expect_equal(sin_4$drainage_area,         40.27062, tolerance = 1e-2)
  expect_equal(sin_4$xs_area,               274.3170, tolerance = 1e-2)
  expect_equal(sin_4$xs_width,              104.0971, tolerance = 1e-2)
  expect_equal(sin_4$xs_depth,              3.467705, tolerance = 1e-2)
  expect_equal(sin_4$fp_area,               1056.571, tolerance = 1e-2)
  expect_equal(sin_4$fp_width,              573.7636, tolerance = 1e-2)
  expect_equal(sin_4$fp_depth,              6.967705, tolerance = 1e-2)
  expect_equal(sin_4$xs_width_depth_ratio,  30.01902, tolerance = 1e-2)
  expect_equal(sin_4$xs_entrenchment_ratio, 5.511811, tolerance = 1e-2)
  expect_equal(sin_4$watersurface_elev,     642.4083, tolerance = 1e-2)
  expect_equal(sin_4$bankfull_elev,         645.8760, tolerance = 1e-2)
  expect_equal(sin_4$floodprone_elev,       649.3760, tolerance = 1e-2)
})
