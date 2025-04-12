fl_pts_plot <- function(fl, fl_pts, dem) {
  plot(dem)
  lines(terra::vect(fl), col = "blue")
  points(vect(filter(fl_pts, POINT_M == min(POINT_M))), col = "green", cex = 5)
  points(vect(filter(fl_pts, POINT_M == max(POINT_M))), col = "red", cex = 5)
  points(fl_pts, col = "white")
}

test_that("check for valid flowline points", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream" 
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 100
  fl_pts <- flowline_points(flowline, dem, station_distance)
  #fl_pts_plot(flowline, fl_pts, dem)
  expect_true(fluvgeo::check_flowline_points(fl_pts))
})
