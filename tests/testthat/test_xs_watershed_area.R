xs_pts_plot <- function(dem, cross_section, xs_pts_class,
                        floodplain_poly, channel_poly) {
  plot(dem)
  lines(cross_section)
  lines(floodplain_poly)
  points(filter(xs_pts_class, floodplain == 1), col = "red", cex = 1)
  lines(channel_poly)
  points(filter(xs_pts_class, channel == 1), col = "green", cex = 2)
}

test_that("check for valid xs points", {
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
  xs <- cross_section(xs, flowline_points)

  #xs_pts_plot(dem, cross_section, xs_pts_class, floodplain_poly, channel_poly)
  expect_true("sf" %in% class(xs_pts_class))
  expect_true("floodplain" %in% colnames(xs_pts_class))
})
