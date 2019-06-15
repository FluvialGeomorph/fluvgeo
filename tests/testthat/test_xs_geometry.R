library(fgm)
context("xs_geometry")

# Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Subset sin_xs_points_df to contain only one cross section (Seq = 4)
sin_xs_points_4 <- sin_xs_points_df[sin_xs_points_df$Seq == 4, ]

# Calculate hydraulic geometry for a single cross section
sin_4 <- xs_geometry(xs_points = sin_xs_points_4, detrend_elevation =  103.5)

sin_4_2 <- xs_geometry(xs_points = sin_xs_points_4, detrend_elevation =  103.5)

test_that("Check parameters", {
  expect_error(xs_geometry(3, 103.5))
  expect_error(xs_geometry(sin_xs_points_4[,-2], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-3], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-4], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-5], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-6], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-7], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-8], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-9], 103))
  expect_error(xs_geometry(sin_xs_points_4[,-10], 103))
  expect_error(xs_geometry(sin_xs_points_4, "a"))
})

test_that("Check that output fields exist by name", {
  expect_true("xs_width"    %in% colnames(sin_4))
  expect_true("xs_depth"    %in% colnames(sin_4))
  expect_true("xs_area"     %in% colnames(sin_4))
  expect_true("ground_elev" %in% colnames(sin_4))
})

test_that("Check that output fields are of the correct data type", {
  expect_true(is.numeric(sin_4$xs_width))
  expect_true(is.numeric(sin_4$xs_depth))
  expect_true(is.numeric(sin_4$xs_area))
  expect_true(is.numeric(sin_4$ground_elev))
})

test_that("Check dimensions from known stream", {
  expect_equal(sin_4$xs_width,    104.0971, tolerance = 1e-2)
  expect_equal(sin_4$xs_depth,    3.4677,   tolerance = 1e-2)
  expect_equal(sin_4$xs_area,     274.3170, tolerance = 1e-2)
  expect_equal(sin_4$ground_elev, 645.8760, tolerance = 1e-2)
})

test_that("Dimensions the same between runs", {
  expect_equal(sin_4$xs_width,    sin_4_2$xs_width, tolerance = 1e-10)
  expect_equal(sin_4$xs_depth,    sin_4_2$xs_depth,   tolerance = 1e-10)
  expect_equal(sin_4$xs_area,     sin_4_2$xs_area, tolerance = 1e-10)
  expect_equal(sin_4$ground_elev, sin_4_2$ground_elev, tolerance = 1e-10)
})
