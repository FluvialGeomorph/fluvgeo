context("xs_geometry")

## Prepare the test data for known site
# Extract the attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Subset sin_xs_points_df to contain only one cross section (Seq = 4)
sin_xs_points_4 <- sin_xs_points_df[sin_xs_points_df$Seq == 4,]

test_that("Check that fields exist by name", {
  expect_true("xs_width" %in% colnames(xs_geometry(sin_xs_points_4, 103.0)))
  expect_true("xs_depth" %in% colnames(xs_geometry(sin_xs_points_4, 103.0)))
  expect_true("xs_area" %in% colnames(xs_geometry(sin_xs_points_4, 103.0)))
  expect_true("bankfull_elev" %in% colnames(xs_geometry(sin_xs_points_4, 103.0)))
})

test_that("Check that fields are of the correct data type", {
  expect_true(is.numeric(xs_geometry(sin_xs_points_4, 103.0)$xs_width))
  expect_true(is.numeric(xs_geometry(sin_xs_points_4, 103.0)$xs_depth))
  expect_true(is.numeric(xs_geometry(sin_xs_points_4, 103.0)$xs_area))
  expect_true(is.numeric(xs_geometry(sin_xs_points_4, 103.0)$bankfull_elev))
})

test_that("Check dimensions from known stream", {
  expect_equal(xs_geometry(xs_points = sin_xs_points_4,
                           bankfull_elevation = 103.5)$xs_width,
               104.0971, tolerance = 1e-2)
  expect_equal(xs_geometry(xs_points = sin_xs_points_4,
                           bankfull_elevation = 103.5)$xs_depth,
               3.4677,   tolerance = 1e-2)
  expect_equal(xs_geometry(xs_points = sin_xs_points_4,
                           bankfull_elevation = 103.5)$xs_area,
               274.3170, tolerance = 1e-2)
  expect_equal(xs_geometry(xs_points = sin_xs_points_4,
                           bankfull_elevation = 103.5)$bankfull_elev,
               645.8760, tolerance = 1e-2)
})
