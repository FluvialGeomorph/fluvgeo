rem_plot <- function(rem, water_surface_poly, flowline) {
  terra::plot(rem)
  terra::polys(water_surface_poly, col = "blue", alpha = 0.5)
  terra::lines(flowline, col = "blue")
}
dem_plot <- function(dem, water_surface_poly, flowline) {
  terra::plot(dem)
  terra::polys(water_surface_poly, col = "blue", alpha = 0.5)
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
  water_surface_elevation <- 105
  wsp <- water_surface_poly(rem, water_surface_elevation, flowline)
  #rem_plot(rem, wsp, flowline)
  #dem_plot(dem, wsp, flowline)
  expect_true("sf" %in% class(wsp))
  expect_true("water_surface_elevation" %in% colnames(wsp))
})
