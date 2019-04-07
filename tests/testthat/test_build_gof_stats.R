library(fgm)
context("build_gof_stats")

# Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Set variable values
streams <- c("Sinsinawa")
regions <- c("Eastern United States", "IN Central Till Plain")
bankfull_elevations <- seq(103, 104, 0.1)

# Call the xs_dimensions function
sin <- xs_dimensions(xs_points = sin_xs_points_df,
                     streams = streams,
                     regions = regions,
                     bankfull_elevations = bankfull_elevations)

# Call the build_gof_stats function
sin_gof <- build_gof_stats(xs_dims = sin,
                           streams = streams,
                           regions = regions,
                           bankfull_elevations = bankfull_elevations)

test_that("Check parameters", {
  expect_error(build_gof_stats(10, streams, regions,
                               bankfull_elevations),
               info = "xs_dims not a data frame")
  expect_error(build_gof_stats(sin[, -1],
                               streams, regions, bankfull_elevations),
               info = "xs_dims is missing reach_name field")
  expect_error(build_gof_stats(sin[, -2], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing cross_section field")
  expect_error(build_gof_stats(sin[, -3], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing xs_type field")
  expect_error(build_gof_stats(sin[, -4], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing bankfull_elevation field")
  expect_error(build_gof_stats(sin[, -5], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing drainage_area field")
  expect_error(build_gof_stats(sin[, -6], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing xs_area field")
  expect_error(build_gof_stats(sin[, -7], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing xs_width field")
  expect_error(build_gof_stats(sin[, -8], streams, regions,
                               bankfull_elevations),
               info = "xs_dims is missing xs_depth field")
  expect_error(build_gof_stats(sin, 8, regions,
                               bankfull_elevations),
               info = "streams is not a character vector")
  expect_error(build_gof_stats(sin, streams, 8,
                               bankfull_elevations),
               info = "regions is not a character vector")
  expect_error(build_gof_stats(sin, streams, regions,
                               "a"),
               info = "bankfull_elevations is not a numeric vector")
})

test_that("Check that fields exist by name", {
  expect_true("reach_name"         %in% colnames(sin_gof))
  expect_true("region"             %in% colnames(sin_gof))
  expect_true("bankfull_elevation" %in% colnames(sin_gof))
  expect_true("rmse_area"          %in% colnames(sin_gof))
  expect_true("rmse_width"         %in% colnames(sin_gof))
  expect_true("rmse_depth"         %in% colnames(sin_gof))
  expect_true("mae_area"           %in% colnames(sin_gof))
  expect_true("mae_width"          %in% colnames(sin_gof))
  expect_true("mae_depth"          %in% colnames(sin_gof))
})

test_that("Check that fields are of the correct data type", {
  expect_true(is.character(sin_gof$reach_name))
  expect_true(is.character(sin_gof$region))
  expect_true(is.numeric(sin_gof$bankfull_elevation))
  expect_true(is.numeric(sin_gof$rmse_area))
  expect_true(is.numeric(sin_gof$rmse_width))
  expect_true(is.numeric(sin_gof$rmse_depth))
})

test_that("Check output dimensionality", {
  expect_equal(length(sin_gof$reach_name),                    22,
               label = "number of reach_name records")
  expect_equal(length(unique(sin_gof$reach_name)),            1,
               label = "number of reaches")
  expect_equal(length(unique(sin_gof$region)),                2,
               label = "number of regions")
  expect_equal(length(unique(sin_gof$bankfull_elevation)),    11,
               label = "number of bankfull elevations")
})
