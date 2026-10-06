xs_line_plot <- function(xs, fl, fl_pts, dem) {
  plot(dem)
  lines(terra::vect(xs), col = "black")
  lines(terra::vect(fl), col = "blue")
  points(vect(sf_line_end_point(fl, "start")), col = "green")
  points(vect(sf_line_end_point(fl, "end")), col = "red")
  #points(fl_pts, col = "white")
  points(vect(sf_line_end_point(xs, "start")), col = "green")
  points(vect(sf_line_end_point(xs, "end")), col = "red")
}

test_that("check for valid cross sections", {
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
  xs_lines <- cross_section(xs, flowline_points, watershed = "skip")
  #xs_plot(xs_lines, flowline, flowline_points, dem)
  expect_true(fluvgeo::check_cross_section(
    xs_lines,
    "station_points",
    watershed = "skip"
  ))
})

test_that("check for flipped cross sections", {
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
  xs_lines <- cross_section(xs_flipped, flowline_points, watershed = "skip")
  #xs_plot(xs_lines, flowline, flowline_points, dem)
  expect_true(fluvgeo::check_cross_section(
    xs_lines,
    "station_points",
    watershed = "skip"
  ))
})

test_that("cross sections honor explicit kilometer Flowline Point measures", {
  line <- sf::st_sf(ReachName = "Reach one", geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 0, 10), ncol = 2, byrow = TRUE)),
    crs = 26915))
  dem <- terra::rast(ncols = 4, nrows = 14, xmin = -2, xmax = 2,
    ymin = -2, ymax = 12, crs = "EPSG:26915")
  terra::values(dem) <- rep(seq(20, 10, length.out = 14), each = 4)
  points <- flowline_points(line, dem, station_distance = 1,
    measure_units = "km")
  points$POINT_M_units <- "km"
  xs <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 5, 1, 5), ncol = 2, byrow = TRUE)),
    crs = 26915))

  result <- cross_section(xs, points, watershed = "skip")

  expect_equal(result$km_to_mouth, result$POINT_M, tolerance = 1e-12)
})
