context("xs_regional_metrics")

## Prepare the test data for known site
# Extract the attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Call the xs_metrics function
sin_4 <- xs_regional_metrics(xs_points = sin_xs_points_df,
                             stream = "Sinsinawa",
                             xs_number = 4,
                             bankfull_elevation = 103.5,
                             region = "Eastern United States")

test_that("Check that fields exist by name", {
  expect_true("reach_name"            %in% colnames(sin_4))
  expect_true("cross_section"         %in% colnames(sin_4))
  expect_true("xs_type"               %in% colnames(sin_4))
  expect_true("bankfull_elevation"    %in% colnames(sin_4))
  expect_true("drainage_area"         %in% colnames(sin_4))
  expect_true("xs_area"               %in% colnames(sin_4))
  expect_true("xs_width"              %in% colnames(sin_4))
  expect_true("xs_depth"              %in% colnames(sin_4))
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
})

# Select from sin_4 the "DEM derived cross section"
sin_4_xs <- sin_4[sin_4$xs_type == "DEM derived cross section", ]

test_that("Check dimensions from known stream", {
  expect_match(sin_4_xs$reach_name,            "Sinsinawa")
  expect_equal(sin_4_xs$cross_section,         4,        tolerance = 1e-2)
  expect_match(sin_4_xs$xs_type,               "DEM derived cross section")
  expect_equal(sin_4_xs$bankfull_elevation,    103.5,    tolerance = 1e-2)
  expect_equal(sin_4_xs$drainage_area,         40.27062, tolerance = 1e-2)
  expect_equal(sin_4_xs$xs_area,               274.3170, tolerance = 1e-2)
  expect_equal(sin_4_xs$xs_width,              104.0971, tolerance = 1e-2)
  expect_equal(sin_4_xs$xs_depth,              3.467705, tolerance = 1e-2)
})

# Select from sin_4 the regional cross section
sin_4_region <- sin_4[sin_4$xs_type == "Eastern United States", ]

test_that("Check dimensions for region", {
  expect_match(sin_4_region$reach_name,            "Sinsinawa")
  expect_equal(sin_4_region$cross_section,         4,        tolerance = 1e-2)
  expect_match(sin_4_region$xs_type,               "Eastern United States")
  expect_equal(sin_4_region$bankfull_elevation,    103.5,    tolerance = 1e-2)
  expect_equal(sin_4_region$drainage_area,         40.27062, tolerance = 1e-2)
  expect_equal(sin_4_region$xs_area,               263.8711, tolerance = 1e-2)
  expect_equal(sin_4_region$xs_width,              60.76679, tolerance = 1e-2)
  expect_equal(sin_4_region$xs_depth,              4.488049, tolerance = 1e-2)
})
