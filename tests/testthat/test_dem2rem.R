test_that("DEM 2 REM", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                        package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream" 
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  rem <- dem2rem(dem, flowline, flowline_points, buffer_distance)
  
  
  plot(dem)
  points(flowline_points)
})