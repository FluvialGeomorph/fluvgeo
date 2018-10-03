context("xs_dimension")

# Call the xs_dimensions function with test data
sin <- xs_dimensions(xs_points = fgm::sin_xs_points@data,
                     streams = c("Sinsinawa"),
                     regions = c("Eastern United States",
                                 "IN Central Till Plain"),
                     bankfull_elevations = seq(103, 104, 0.1))

test_that("Check that fields exist by name", {
  expect_true("reach_name"            %in% colnames(sin))
  expect_true("cross_section"         %in% colnames(sin))
  expect_true("xs_type"               %in% colnames(sin))
  expect_true("bankfull_elevation"    %in% colnames(sin))
  expect_true("drainage_area"         %in% colnames(sin))
  expect_true("xs_area"               %in% colnames(sin))
  expect_true("xs_width"              %in% colnames(sin))
  expect_true("xs_depth"              %in% colnames(sin))
})

test_that("Check that fields are of the correct data type", {
  expect_true(is.character(sin$reach_name))
  expect_true(is.numeric(sin$cross_section))
  expect_true(is.character(sin$xs_type))
  expect_true(is.numeric(sin$bankfull_elevation))
  expect_true(is.numeric(sin$drainage_area))
  expect_true(is.numeric(sin$xs_area))
  expect_true(is.numeric(sin$xs_width))
  expect_true(is.numeric(sin$xs_depth))
})

test_that("Check output dimensionality", {
  expect_equal(length(sin$reach_name),                    88,
               label = "number of reach_name records")
  expect_equal(length(unique(sin$reach_name)),            1,
               label = "number of reaches")
  expect_equal(length(unique(sin$cross_section)),         2,
               label = "number of cross sections")
  expect_equal(length(unique(sin$xs_type)),               3,
               label = "number of xs types")
  expect_equal(length(unique(sin$bankfull_elevation)),    11,
               label = "number of bankfull elevations")
  expect_equal(length(unique(sin$drainage_area)),         2,
               label = "number of drainage areas")
  expect_equal(length(unique(sin$xs_area)),               26,
               label = "number of unique xs_area records")
})
