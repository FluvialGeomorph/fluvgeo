library(fluvgeo)
library(dplyr)
library(sf)
context("cross_section_dimensions")

# Get feature class test data
xs_dims_fc    <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "xs_250_25_dims")

# Convert to sf
xs_dims_sf <- fluvgeo::fc2sf(xs_dims_fc)

# Select fields available at level 1
xs_dims_L1_sf <- dplyr::select(xs_dims_sf, 1:15)

# Convert to sf
xs <- sf::as_Spatial(xs_dims_L1_sf)

# Set parameters
lead_n = 1
use_smoothing = TRUE
loess_span = 0.5
vert_units = "ft"

# Calculate cross section dimensions
xs_dims_L1 <- cross_section_dimensions_L1(xs = xs,
                                          lead_n = lead_n,
                                          use_smoothing = use_smoothing,
                                          loess_span = loess_span,
                                          vert_units = vert_units)

# Calculate cross section dimensions
xs_dims_L2 <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
                              xs_points = fluvgeo::sin_riffle_channel_points_sp,
                              bankfull_elevation = 103,
                              lead_n = 1,
                              use_smoothing = TRUE,
                              loess_span = 0.5,
                              vert_units = "ft")

test_that("check cross section dimensions level_1", {
  expect_true(check_cross_section_dimensions(xs_dims_L1, "level_1"))
})

test_that("check cross section dimensions", {
  expect_true(check_cross_section_dimensions(xs_dims_L2,
                                             "cross_section_dimensions"))
})
