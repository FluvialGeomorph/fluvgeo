library(fluvgeo)
library(sp)
library(rgdal)
context("slope_sinuosity")

# horiz. units: meters

# Get sp test data
sin_fl_pts_sp         <- fluvgeo::sin_flowline_points_sp
sin_riffle_channel_sp <- fluvgeo::sin_riffle_channel_sp

sin_fl_pts_ss         <- slope_sinuosity(sin_fl_pts_sp,
                                         lead_n = 100, lag_n = 0,
                                         loess_span = 0.5,
                                         vert_units = "ft")

sin_riffle_channel_ss <- slope_sinuosity(sin_riffle_channel_sp,
                                         lead_n = 3, lag_n = 0,
                                         loess_span = 0.5,
                                         vert_units = "ft")

# Get feature class test data
fl_pts_m_fc  <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                      package = "fluvgeo"),
                          "flowline_points")
xs_m_fc      <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                      package = "fluvgeo"),
                          "xs_250_25")

# Convert to sf and then to sp
fl_pts_m_sf <- fluvgeo::fc2sf(fl_pts_m_fc)
xs_m_sf     <- fluvgeo::fc2sf(xs_m_fc)
fl_pts_m_sp <- sf::as_Spatial(sf::st_zm(fl_pts_m_sf))
xs_m_sp     <- sf::as_Spatial(xs_m_sf)

fl_pts_m_sp_ss <- slope_sinuosity(fl_pts_m_sp,
                                  lead_n = 100, lag_n = 0,
                                  loess_span = 0.5,
                                  vert_units = "ft")

xs_m_sp_ss <- slope_sinuosity(xs_m_sp,
                              lead_n = 1, lag_n = 0,
                              loess_span = 0.5,
                              vert_units = "ft")

# horiz. units: us-ft

# Get feature class test data
fl_pts_ft_fc <- file.path(system.file("extdata", "state_plane.gdb",
                                      package = "fluvgeo"),
                          "flowline_points")
xs_ft_fc     <- file.path(system.file("extdata", "state_plane.gdb",
                                      package = "fluvgeo"),
                          "XS_R1_2016")

# Convert to sf and then to sp
fl_pts_ft_sf <- fluvgeo::fc2sf(fl_pts_ft_fc)
xs_ft_sf     <- fluvgeo::fc2sf(xs_ft_fc)
fl_pts_ft_sp <- sf::as_Spatial(sf::st_zm(fl_pts_ft_sf))
xs_ft_sp     <- sf::as_Spatial(sf::st_zm(xs_ft_sf))

fl_pts_ft_sp_ss <- slope_sinuosity(fl_pts_ft_sp,
                                   lead_n = 100, lag_n = 0,
                                   loess_span = 0.5,
                                   vert_units = "ft")

xs_ft_sp_ss <- slope_sinuosity(xs_ft_sp,
                               lead_n = 6, lag_n = 0,
                               loess_span = 0.5,
                               vert_units = "ft")

test_that("Check sp data horz units meters", {
  expect_true(check_slope_sinuosity(sin_fl_pts_ss))
  expect_true(check_slope_sinuosity(sin_riffle_channel_ss))
})

test_that("Check sf data horz units meters", {
  expect_true(check_slope_sinuosity(fl_pts_m_sp_ss))
  expect_true(check_slope_sinuosity(xs_m_sp_ss))
})

test_that("Check sf data horz units us-ft", {
  expect_true(check_slope_sinuosity(fl_pts_ft_sp_ss))
  expect_true(check_slope_sinuosity(xs_ft_sp_ss))
})
