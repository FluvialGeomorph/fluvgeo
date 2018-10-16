context("build_gof_stats")

# Call the xs_dimensions function with test data
sin <- xs_dimensions(xs_points = fgm::sin_xs_points@data,
                     streams = c("Sinsinawa"),
                     regions = c("Eastern United States",
                                 "IN Central Till Plain"),
                     bankfull_elevations = seq(103, 104, 0.1))

# Call the build_gof_stats function
sin_gof <- build_gof_stats(xs_dims = sin,
                           streams = c("Sinsinawa"),
                           regions = c("Eastern United States",
                                       "IN Central Till Plain"),
                           bankfull_elevations = seq(103, 104, 0.1))

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
