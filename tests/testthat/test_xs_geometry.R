context("xs_geometry")

## Prepare the test data for known site
# Extract the attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Subset sin_xs_points_df to contain only one cross section (Seq = 4)
sin_xs_points_4 <- sin_xs_points_df[sin_xs_points_df$Seq == 4,]

sin_4 <- xs_geometry(xs_points = sin_xs_points_4,
                     detrend_elevation =  103.5)

test_that("Check that fields exist by name", {
  expect_true("xs_width"    %in% colnames(sin_4))
  expect_true("xs_depth"    %in% colnames(sin_4))
  expect_true("xs_area"     %in% colnames(sin_4))
  expect_true("ground_elev" %in% colnames(sin_4))
})

test_that("Check that fields are of the correct data type", {
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
