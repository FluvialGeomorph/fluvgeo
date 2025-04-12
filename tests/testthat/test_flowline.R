fl_dir_plot <- function(og_fl, fixed_fl, dem) {
  plot(dem)
  lines(vect(og_fl), col = "blue")
  points(vect(sf_line_end_point(og_fl, "start")), col = "green", cex = 5)
  points(vect(sf_line_end_point(og_fl, "end")), col = "red", cex = 5)
  points(vect(sf_line_end_point(fixed_fl, "start")), col = "green")
  points(vect(sf_line_end_point(fixed_fl, "end")), col = "red")
}

test_that("fl_mapedit digitized in the upstream direction", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  flowline <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream" 
  dem <- get_dem(flowline)
  fl <- flowline(flowline, reach_name, dem)
  #fl_dir_plot(flowline, fl, dem)
  start_z <- sf_get_z(sf_line_end_point(fl, "start"), dem)$z
  end_z   <- sf_get_z(sf_line_end_point(fl, "end"), dem)$z
  expect_true(fluvgeo::check_flowline(fl, step = "create_flowline"))
  expect_true(start_z <= end_z)
})
test_that("fl_mapedit digitized in the downstream direction", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                        package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  # Purposely reverse a correctly digitized flowline to test function
  flowline <- sf_line_reverse(fl_3857)
  reach_name <- "current stream"
  dem <- get_dem(flowline)
  fl <- flowline(flowline, reach_name, dem)
  #fl_dir_plot(flowline, fl, dem)
  start_z <- sf_get_z(sf_line_end_point(fl, "start"), dem)$z
  end_z   <- sf_get_z(sf_line_end_point(fl, "end"), dem)$z
  expect_true(fluvgeo::check_flowline(fl, step = "create_flowline"))
  expect_true(start_z <= end_z)
})