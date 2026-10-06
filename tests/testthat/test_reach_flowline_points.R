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

  points <- reach_flowline_points(lines, dem)

  expect_s3_class(points, "sf")
  expect_equal(sf::st_crs(points), sf::st_crs(lines))
  expect_equal(points$POINT_M_units, rep("km", nrow(points)))
  expect_equal(points$km_to_mouth_units, rep("km", nrow(points)))
  expect_equal(points$km_to_mouth, points$POINT_M)
  expect_equal(points$POINT_M_uncalibrated, points$POINT_M)
  expect_equal(points$calibration_diff, rep(0, nrow(points)))
  expect_equal(points$station_distance_m, rep(1, nrow(points)))
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

test_that("Reach Flowline Points accept a legacy kilometer offset", {
  crs <- "EPSG:32615"
  line <- sf::st_sf(reach_id = "tributary", ReachName = "Tributary",
    reach_order = 1L, geometry = sf::st_sfc(sf::st_linestring(matrix(
      c(500000, 4500000, 500010, 4500000), ncol = 2, byrow = TRUE)), crs = crs))
  dem <- terra::init(terra::rast(xmin = 499999, xmax = 500011,
    ymin = 4499999, ymax = 4500001, resolution = 1, crs = crs), "x")

  points <- reach_flowline_points(line, dem, station_distance = 5,
    measure_origin = "STUDY_AREA_OUTLET", measure_offset_km = 2.5)

  expect_equal(range(points$km_to_mouth), c(2.5, 2.51), tolerance = 1e-6)
  expect_equal(range(points$distance_to_stream_outlet_m), c(0, 10),
    tolerance = 1e-6)
  expect_equal(unique(points$stream_offset_km), 2.5)
})

test_that("Study Area Flowline Points inherit tributary stationing at confluences", {
  crs <- "EPSG:32615"
  main <- sf::st_sf(reach_id = "main-reach", ReachName = "Main",
    reach_order = 1L, geometry = sf::st_sfc(sf::st_linestring(matrix(
      c(500000, 4500000, 500020, 4500000), ncol = 2, byrow = TRUE)), crs = crs))
  tributary <- sf::st_sf(reach_id = "trib-reach", ReachName = "Tributary",
    reach_order = 1L, geometry = sf::st_sfc(sf::st_linestring(matrix(
      c(500010, 4500000.5, 500010, 4500010), ncol = 2, byrow = TRUE)), crs = crs))
  main_corridor <- sf::st_buffer(sf::st_sfc(sf::st_linestring(matrix(
    c(500000, 4500000, 500020, 4500000), ncol = 2, byrow = TRUE)), crs = crs), 2)
  tributary_corridor <- sf::st_buffer(sf::st_sfc(sf::st_linestring(matrix(
    c(500010, 4500000, 500010, 4500010), ncol = 2, byrow = TRUE)), crs = crs), 1)
  corridors <- sf::st_sf(stream_id = c("main", "tributary"),
    stream_name = c("Mainstem", "Tributary"),
    geometry = c(main_corridor, tributary_corridor))
  dem <- terra::init(terra::rast(xmin = 499999, xmax = 500021,
    ymin = 4499999, ymax = 4500011, resolution = 1, crs = crs), "x") +
    terra::init(terra::rast(xmin = 499999, xmax = 500021,
      ymin = 4499999, ymax = 4500011, resolution = 1, crs = crs), "y")

  result <- study_area_flowline_points(
    flowlines = list(main = main, tributary = tributary),
    dems = list(main = dem, tributary = dem),
    stream_corridors = corridors, station_distance = 1)

  expect_s3_class(result$points, "sf")
  expect_s3_class(result$connections, "sf")
  expect_equal(result$connections$stream_id[result$connections$is_study_outlet],
    "main")
  tributary_start <- min(result$points$km_to_mouth[
    result$points$stream_id == "tributary"])
  expect_equal(tributary_start, .01, tolerance = 1e-6)
  expect_equal(result$connections$connection_distance_m[
    result$connections$stream_id == "tributary"], .5, tolerance = 1e-6)
  expect_true(check_flowline_points(result$points, "fgstudio_replacement"))
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
