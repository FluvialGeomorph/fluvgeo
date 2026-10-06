test_that("Reach Flowline Points retain CRS and continuous outlet measures", {
  crs <- "EPSG:32615"
  lines <- sf::st_sf(
    reach_id = c("downstream", "upstream"),
    ReachName = c("Lower", "Upper"),
    reach_order = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(500000, 4500000, 500010, 4500000), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(500010, 4500000, 500020, 4500000), ncol = 2, byrow = TRUE)),
      crs = crs))
  dem <- terra::rast(xmin = 499999, xmax = 500021, ymin = 4499999,
    ymax = 4500001, resolution = 1, crs = crs)
  terra::values(dem) <- seq_len(terra::ncell(dem))

  points <- reach_flowline_points(lines, dem, station_distance = 5)

  expect_s3_class(points, "sf")
  expect_equal(sf::st_crs(points), sf::st_crs(lines))
  expect_equal(points$POINT_M_units, rep("km", nrow(points)))
  expect_equal(points$km_to_mouth_units, rep("km", nrow(points)))
  expect_equal(points$km_to_mouth, points$POINT_M)
  expect_equal(points$distance_to_stream_outlet_m, points$POINT_M * 1000)
  expect_equal(points$measure_origin, rep("SELECTED_STREAM_OUTLET", nrow(points)))
  expect_equal(min(points$POINT_M), 0)
  expect_equal(max(points$POINT_M), 0.02, tolerance = 1e-6)
  boundary <- points[abs(points$POINT_M - 0.01) < 1e-6, ]
  expect_equal(nrow(boundary), 2L)
  expect_setequal(boundary$reach_id, c("downstream", "upstream"))
  expect_true(all(is.finite(points$Z)))
  expect_true(check_flowline_points(points, contract = "fgstudio_replacement"))
})

test_that("Reach Flowline Points fail closed on disconnected Reach lines", {
  crs <- "EPSG:32615"
  lines <- sf::st_sf(
    reach_id = c("one", "two"), ReachName = c("One", "Two"), reach_order = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(500000, 4500000, 500010, 4500000), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(500011, 4500000, 500020, 4500000), ncol = 2, byrow = TRUE)),
      crs = crs))
  dem <- terra::rast(xmin = 499999, xmax = 500021, ymin = 4499999,
    ymax = 4500001, resolution = 1, crs = crs)
  terra::values(dem) <- 1

  expect_error(reach_flowline_points(lines, dem), "share an exact endpoint")
})

test_that("existing R metre measures and ArcPy replacement kilometre measures are explicit", {
  crs <- "EPSG:32615"
  line <- sf::st_sf(ReachName = "Reach", geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(500000, 4500000, 500010, 4500000),
      ncol = 2, byrow = TRUE)), crs = crs))
  dem <- terra::rast(xmin = 499999, xmax = 500011, ymin = 4499999,
    ymax = 4500001, resolution = 1, crs = crs)
  terra::values(dem) <- seq_len(terra::ncell(dem))

  current_r <- flowline_points(line, dem, station_distance = 5)
  replacement <- flowline_points(line, dem, station_distance = 5,
    measure_units = "km")

  expect_equal(max(current_r$POINT_M), 10, tolerance = 1e-6)
  expect_equal(max(current_r$km_to_mouth), .01, tolerance = 1e-6)
  expect_equal(max(replacement$POINT_M), .01, tolerance = 1e-6)
  expect_equal(replacement$POINT_M, replacement$km_to_mouth)
})
