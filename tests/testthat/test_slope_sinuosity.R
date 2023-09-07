library(fluvgeo)
library(sp)
context("slope_sinuosity")

# horiz. units: meters

# Get sf test data
sin_fl_pts_sf         <- fluvgeo::sin_flowline_points_sf
sin_riffle_channel_sf <- fluvgeo::sin_riffle_channel_sf

sin_fl_pts_ss         <- slope_sinuosity(sin_fl_pts_sf,
                                         lead_n = 50, lag_n = 50,
                                         loess_span = 0.5,
                                         vert_units = "ft")

sin_riffle_channel_ss <- slope_sinuosity(sin_riffle_channel_sf,
                                         lead_n = 1, lag_n = 1,
                                         loess_span = 0.5,
                                         vert_units = "ft")

# Get feature class test data
fl_pts_m_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                      package = "fluvgeo"),
                          "feature_dataset/flowline_points")
xs_m_fc      <- file.path(system.file("extdata", "y2016_R1.gdb",
                                      package = "fluvgeo"),
                          "feature_dataset/xs_50")

# Convert to sf
fl_pts_m_sf <- fluvgeo::fc2sf(fl_pts_m_fc)
xs_m_sf     <- fluvgeo::fc2sf(xs_m_fc)


fl_pts_m_sf_ss <- slope_sinuosity(fl_pts_m_sf,
                                  lead_n = 50, lag_n = 50,
                                  loess_span = 0.5,
                                  vert_units = "ft")

xs_m_sf_ss <- slope_sinuosity(xs_m_sf,
                              lead_n = 1, lag_n = 1,
                              loess_span = 0.5,
                              vert_units = "ft")

# horiz. units: us-ft

# Get feature class test data
fl_pts_ft_fc <- file.path(system.file("extdata", "state_plane.gdb",
                                      package = "fluvgeo"),
                          "feature_dataset/flowline_points")
xs_ft_fc     <- file.path(system.file("extdata", "state_plane.gdb",
                                      package = "fluvgeo"),
                          "feature_dataset/XS_R1_2016")

# Convert to sf
fl_pts_ft_sf <- fluvgeo::fc2sf(fl_pts_ft_fc)
xs_ft_sf     <- fluvgeo::fc2sf(xs_ft_fc)


fl_pts_ft_sf_ss <- slope_sinuosity(fl_pts_ft_sf,
                                   lead_n = 50, lag_n = 50,
                                   loess_span = 0.5,
                                   vert_units = "ft")

xs_ft_sf_ss <- slope_sinuosity(xs_ft_sf,
                               lead_n = 3, lag_n = 3,
                               loess_span = 0.5,
                               vert_units = "ft")


test_that("Check sf data horz units meters", {
  expect_true(check_slope_sinuosity(fl_pts_m_sf_ss))
  expect_true(check_slope_sinuosity(xs_m_sf_ss))
})

test_that("Check sf data horz units us-ft", {
  expect_true(check_slope_sinuosity(fl_pts_ft_sf_ss))
  expect_true(check_slope_sinuosity(xs_ft_sf_ss))
})

