library(fluvgeo)
library(sp)
library(rgdal)
context("slope_sinuosity")

# Call the slope_sinuosity function for a flowline
sin_flowline_ss <- slope_sinuosity(fluvgeo::sin_flowline_points_sp,
                                   lead_n = 1000, lag_n = 0,
                                   loess_span = 0.5,
                                   vert_units = "ft")

# Call the slope_sinuosity function for a cross section
sin_riffle_channel_ss <- slope_sinuosity(fluvgeo::sin_riffle_channel_sp,
                                         lead_n = 1, lag_n = 0,
                                         loess_span = 0.5,
                                         vert_units = "ft")

# Reproject to a CRS with units = "us-ft"
sin_riffle_channel_sp_ft <- sp::spTransform(fluvgeo::sin_riffle_channel_sp,
                                              sp::CRS(SRS_string = "EPSG:6457"))

sin_riffle_channel_ss_ft <- slope_sinuosity(sin_riffle_channel_sp_ft,
                                         lead_n = 1, lag_n = 0,
                                         loess_span = 0.5,
                                         vert_units = "ft")

# Get feature class test data
xs_fc    <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "xs_250_25")
# Convert to sf and then to sp
xs_sf <- fluvgeo::fc2sf(xs_fc)
xs_sp <- sf::as_Spatial(xs_sf)

xs_dims_sfsp <- slope_sinuosity(xs_sp,
                                lead_n = 1, lag_n = 0,
                                loess_span = 0.5,
                                vert_units = "ft")

test_that("Check output data structures", {
  expect_true(check_slope_sinuosity(sin_flowline_ss))
  expect_true(check_slope_sinuosity(sin_riffle_channel_ss))
  expect_true(check_slope_sinuosity(sin_riffle_channel_ss_ft))
  expect_true(check_slope_sinuosity(xs_dims_sfsp))
})
