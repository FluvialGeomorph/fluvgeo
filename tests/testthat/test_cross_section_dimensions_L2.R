library(fluvgeo)
library(dplyr)
library(sf)
context("cross_section_dimensions_L2")

# Get feature class test data
xs_dims_fc    <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "feature_dataset/xs_50")
xs_points_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "feature_dataset/xs_50_points")

# Convert to sf
xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
xs_points_sf <- fluvgeo::fc2sf(xs_points_fc)

# Convert from sf to sp
xs        <- sf::as_Spatial(xs_dims_sf)
xs_points <- sf::as_Spatial(sf::st_zm(xs_points_sf, what = "ZM"))

# Set parameters
bankfull_elevation <- 104
lead_n <- 3
use_smoothing <- TRUE
loess_span <- 0.5
vert_units <- "ft"

# Calculate cross section dimensions
xs_dims_1 <- cross_section_dimensions_L2(xs = xs,
                                      xs_points = xs_points,
                                      bankfull_elevation = bankfull_elevation,
                                      lead_n = lead_n,
                                      use_smoothing = use_smoothing,
                                      loess_span = loess_span,
                                      vert_units = vert_units)

# Calculate cross section dimensions
xs_dims_2 <- cross_section_dimensions_L2(xs = fluvgeo::sin_riffle_channel_sp,
                              xs_points = fluvgeo::sin_riffle_channel_points_sp,
                              bankfull_elevation = bankfull_elevation,
                              lead_n = lead_n,
                              use_smoothing = use_smoothing,
                              loess_span = loess_span,
                              vert_units = vert_units)

test_that("check cross section dimensions level_1", {
  expect_true(check_cross_section_dimensions(xs_dims_1,
                                             "cross_section_dimensions"))
})

test_that("check cross section dimensions", {
  expect_true(check_cross_section_dimensions(xs_dims_2,
                                             "cross_section_dimensions"))
})
