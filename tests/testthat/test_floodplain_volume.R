channel_plot <- function(flowline, rem, bankfull) {
  terra::plot(rem < bankfull)
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
  rem <- detrend$rem
  trend <- detrend$trend
  watersurface <- trend + 2                     # water depth = baseline + 2 ft
  #channel_plot(flowline, rem, 102)
  fpv <- floodplain_volume(dem, watersurface)
  expect_true(is.numeric(fpv))
})
