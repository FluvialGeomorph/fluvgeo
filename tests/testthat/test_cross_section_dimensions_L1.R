library(fluvgeo)
library(dplyr)
library(sf)
context("cross_section_dimensions_L1")

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
xs_dims <- cross_section_dimensions_L1(xs = xs,
                                       lead_n = lead_n,
                                       use_smoothing = use_smoothing,
                                       loess_span = loess_span,
                                       vert_units = vert_units)


