context("xs_regional_metrics")

# Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Call the xs_metrics function
sin_4 <- xs_regional_metrics(xs_points = sin_xs_points_df,
                             stream = "Sinsinawa",
                             xs_number = 4,
                             bankfull_elevation = 103.5,
                             region = "Eastern United States")

test_that("Check parameters", {
  expect_error(xs_regional_metrics(10, "Sinsinawa", 4, 103.5),
               info = "xs_points not a dataframe")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-2], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the Seq field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-3], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the POINT_X field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-4], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the POINT_Y field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-5], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the POINT_M field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-6], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the Watershed_Area_SqMile field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-7], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the km_to_mouth field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-8], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the DEM_Z field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-9], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the Detrend_DEM_Z field")
  expect_error(xs_regional_metrics(sin_xs_points_df[,-10], "Sinsinawa",
                                   4, 103.5, "MA"),
               info = "xs_points is missing the ReachName field")
  expect_error(xs_regional_metrics(sin_xs_points_df, "",
                                   4, 103.5, "MA"),
               info = "stream is an empty string")
  expect_error(xs_regional_metrics(sin_xs_points_df, c("stream1", "stream2"),
                                   4, 103.5, "MA"),
               info = "stream is a vector of length > 1")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4.1, 103.5, "MA"),
               info = "xs_number is not an integer")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   c(4, 5), 103.5, "MA"),
               info = "xs_number is a vector of length > 1")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, "a", "MA"),
               info = "bankfull_elevation is not numeric")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, c(103.5, 103.6), "MA"),
               info = "bankfull_elevation is a vector of length > 1")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, 103.5, 8),
               info = "region is a chaacter")
  expect_error(xs_regional_metrics(sin_xs_points_df, "Sinsinawa",
                                   4, 103.5, c("MA", "VT")),
               info = "region ia vector of length > 1")
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
