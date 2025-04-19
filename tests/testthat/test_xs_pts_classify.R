xs_pts_plot <- function(dem, cross_section, xs_pts_class,
                        floodplain_poly, channel_poly) {
  plot(dem)
  lines(cross_section)
  lines(floodplain_poly)
  points(filter(xs_pts_class, floodplain == 1), col = "red", cex = 1)
  lines(channel_poly)
  points(filter(xs_pts_class, channel == 1), col = "green", cex = 2)
}

test_that("check for valid flowline points", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(xs)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  cross_section <- cross_section(xs, flowline_points)
  station_distance = 5
  xs_pts <- cross_section_points(cross_section, dem, rem, station_distance)
  channel_wse <- 103
  channel_poly <- water_surface_poly(rem, channel_wse, flowline)
  floodplain_wse <- 112
  floodplain_poly <- water_surface_poly(rem, floodplain_wse, flowline)
  buffer_distance <- 5
  xs_pts_class <- xs_pts_classify(xs_pts, channel_poly, floodplain_poly,
                                  buffer_distance)
  #xs_pts_plot(dem, xs_pts_class)
  expect_true(sf %in% class(xs_pts_class))
})
