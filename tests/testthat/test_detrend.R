rem_plot <- function(flowline, rem, detrend_elevation) {
  terra::plot(rem < detrend_elevation)
  terra::lines(flowline, col = "blue")
}
trend_plot <- function(flowline, trend) {
  terra::plot(trend)
  terra::lines(flowline, col = "blue")
}

test_that("check detrend", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  #rem_plot(flowline, detrend$rem, 102)
  #trend_plot(flowline, detrend$trend)
  expect_true("SpatRaster" %in% class(detrend$rem))
  expect_true("SpatRaster" %in% class(detrend$trend))
})
