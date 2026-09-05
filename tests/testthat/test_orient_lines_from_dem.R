direction_test_inputs <- function() {
  list(
    lines = suppressWarnings(sf::st_read(
      system.file("extdata", "testing_data.gdb", package = "fluvgeodata"),
      layer = "stream_network", quiet = TRUE
    )),
    dem = terra::rast(system.file("extdata", "dem_1m.tif", package = "fluvgeodata"))
  )
}

test_that("DEM orientation preserves features and is idempotent", {
  inputs <- direction_test_inputs()
  original <- inputs$lines
  result <- orient_lines_from_dem(original, inputs$dem)
  evidence <- result$direction
  expect_named(result, c("lines", "direction"))
  expect_identical(inputs$lines, original)
  expect_identical(sf::st_drop_geometry(result$lines), sf::st_drop_geometry(original))
  expect_equal(sf::st_geometry_type(result$lines), sf::st_geometry_type(original))
  expect_equal(sf::st_crs(result$lines), sf::st_crs(original))
  expect_equal(evidence$input_row, seq_len(nrow(original)))
  expect_equal(sum(evidence$action == "KEEP"), 13L)
  expect_equal(sum(evidence$action == "REVERSE"), 33L)
  expect_equal(sum(evidence$action == "UNRESOLVED"), 53L)
  expect_equal(sum(evidence$reason_code == "ENDPOINT_OUTSIDE_DEM"), 46L)
  expect_equal(sum(evidence$reason_code == "ENDPOINT_DEM_NODATA"), 4L)
  expect_equal(sum(evidence$reason_code == "EQUAL_ENDPOINT_ELEVATION"), 3L)
  endpoint_status <- c(evidence$start_sample_status, evidence$end_sample_status)
  expect_equal(sum(endpoint_status == "OUTSIDE_DEM_EXTENT"), 90L)
  expect_equal(sum(endpoint_status == "DEM_NODATA"), 8L)
  expect_equal(sum(endpoint_status == "AVAILABLE"), 100L)
  for (i in which(evidence$action == "REVERSE")) {
    source_coordinates <- sf::st_geometry(original)[[i]][[1L]]
    expect_equal(sf::st_geometry(result$lines)[[i]][[1L]],
                 source_coordinates[nrow(source_coordinates):1L, ])
  }
  again <- orient_lines_from_dem(result$lines, inputs$dem)
  expect_equal(sf::st_geometry(again$lines), sf::st_geometry(result$lines))
  expect_false(any(again$direction$action == "REVERSE"))
  expect_true(all(again$direction$start_elevation[again$direction$action == "KEEP"] <
                    again$direction$end_elevation[again$direction$action == "KEEP"]))
})

test_that("equal or missing DEM values and multipart lines remain unresolved", {
  inputs <- direction_test_inputs()
  assessment <- orient_lines_from_dem(inputs$lines, inputs$dem)
  line <- inputs$lines[which(assessment$direction$action == "REVERSE")[1L], ]
  # Deliberate in-memory changes to the retained raster exercise missing/flat evidence.
  flat <- terra::init(inputs$dem, 1)
  missing <- terra::init(inputs$dem, NA_real_)
  for (dem in list(flat, missing)) {
    result <- orient_lines_from_dem(line, dem)
    expect_equal(result$direction$action, "UNRESOLVED")
    expect_equal(sf::st_geometry(result$lines), sf::st_geometry(line))
  }
  expect_equal(orient_lines_from_dem(line, flat)$direction$reason_code,
               "EQUAL_ENDPOINT_ELEVATION")
  expect_equal(orient_lines_from_dem(line, missing)$direction$reason_code,
               "ENDPOINT_DEM_NODATA")
  multipart <- line
  part <- sf::st_geometry(line)[[1L]][[1L]]
  sf::st_geometry(multipart) <- sf::st_sfc(
    sf::st_multilinestring(list(part, part)), crs = sf::st_crs(line)
  )
  result <- orient_lines_from_dem(multipart, inputs$dem)
  expect_equal(result$direction$reason_code, "MULTIPART_GEOMETRY")
  expect_equal(sf::st_geometry(result$lines), sf::st_geometry(multipart))
})

test_that("orientation validates raster bands and coordinate systems", {
  inputs <- direction_test_inputs()
  expect_error(orient_lines_from_dem(inputs$lines, c(inputs$dem, inputs$dem)), "single-band")
  expect_error(orient_lines_from_dem(sf::st_transform(inputs$lines, 3857), inputs$dem), "same CRS")
  expect_error(orient_lines_from_dem(sf::st_transform(inputs$lines, 4326), inputs$dem), "projected")
})

test_that("flowline uses the shared direction method with a local DEM", {
  inputs <- direction_test_inputs()
  selected <- which(orient_lines_from_dem(inputs$lines, inputs$dem)$direction$action == "REVERSE")[1L]
  line <- inputs$lines[selected, ]
  # Isolate the existing GeoJSON-specific CRS repair wrapper. The new shared
  # orientation method and DEM sampling run unmocked on the direct UTM pair.
  testthat::local_mocked_bindings(sf_fix_crs = identity, .package = "fluvgeo")
  dem <- inputs$dem
  expected <- orient_lines_from_dem(line, dem)
  result <- flowline(line, "Retained example", dem)
  expect_equal(sf::st_geometry(result), sf::st_geometry(expected$lines))
  expect_equal(result$ReachName, "Retained example")
  flat <- terra::init(dem, 1)
  expect_warning(unchanged <- flowline(line, "Retained example", flat), "direction is unresolved")
  expect_equal(sf::st_geometry(unchanged), sf::st_geometry(line))
})
