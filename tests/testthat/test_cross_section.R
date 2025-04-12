xs_plot <- function(xs, fl, fl_pts, dem) {
  plot(dem)
  lines(terra::vect(xs), col = "black")
  lines(terra::vect(fl), col = "blue")
  points(vect(sf_line_end_point(fl, "start")), col = "green")
  points(vect(sf_line_end_point(fl, "end")), col = "red")
  #points(fl_pts, col = "white")
  points(vect(sf_line_end_point(xs, "start")), col = "green")
  points(vect(sf_line_end_point(xs, "end")), col = "red")
}

test_that("check for valid flowline points", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  xs_lines <- cross_section(xs, flowline_points)
  #xs_plot(xs_lines, flowline, flowline_points, dem)
  expect_true(fluvgeo::check_cross_section(xs_lines, "station_points"))
})

test_that("check for valid flowline points", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  xs_flipped <- sf_line_reverse(xs)
  xs_lines <- cross_section(xs_flipped, flowline_points)
  #xs_plot(xs_lines, flowline, flowline_points, dem)
  expect_true(fluvgeo::check_cross_section(xs_lines, "station_points"))
})
