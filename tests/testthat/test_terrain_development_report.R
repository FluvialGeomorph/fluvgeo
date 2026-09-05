terrain_test_context <- function() {
  aid <- .fg_generate_uuid(1); sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1)
  list(study_area = sf::st_sf(study_area_id = aid, study_area_name = "Synthetic AOI",
    geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 100, ymax = 100), crs = 26914))),
    streams = data.frame(stream_id = sid, study_area_id = aid, stream_name = "Synthetic Creek"),
    reaches = data.frame(reach_id = rid, stream_id = sid, reach_name = "R1"),
    survey_events = data.frame(survey_event_id = .fg_generate_uuid(3), reach_id = rid,
      survey_year = c(2006L, 2010L, 2016L), survey_month = c(NA_integer_, 2L, 2L), survey_day = c(NA_integer_, NA_integer_, 29L)))
}

test_that("scope definition works before a network or DEM exists", {
  x <- terrain_test_context()
  before <- x
  s <- do.call(terrain_development_summary, x)
  expect_identical(x, before)
  expect_null(s$segments)
  expect_equal(s$surveys$date_label, c("2006", "2010-02", "2016-02-29"))
  expect_equal(s$reaches$stream_name, "Synthetic Creek")
  expect_true(any(grepl("scope-definition", s$gaps)))
  expect_true(any(grepl("DEM not supplied", s$gaps)))
  empty <- terrain_development_summary()
  expect_null(empty$study_area)
  expect_equal(nrow(empty$surveys), 0L)
  expect_true(any(grepl("not substituted", empty$gaps)))
})

test_that("context ownership and partial dates are not inferred", {
  x <- terrain_test_context()
  x$reaches$stream_id <- .fg_generate_uuid(1)
  expect_error(do.call(terrain_development_summary, x), "parent Streams")
  x <- terrain_test_context()
  x$survey_events$reach_id[1] <- .fg_generate_uuid(1)
  expect_error(do.call(terrain_development_summary, x), "parent Reaches")
  x <- terrain_test_context()
  x$survey_events$survey_year[3] <- 2015L
  expect_error(do.call(terrain_development_summary, x))
  x <- terrain_test_context()
  x$survey_events$survey_day[1] <- 1L
  expect_error(do.call(terrain_development_summary, x), "missing month")
  x <- terrain_test_context()
  x$streams$study_area_id <- .fg_generate_uuid(1)
  expect_error(do.call(terrain_development_summary, x), "ownership mismatch")
  x <- terrain_test_context()
  sf::st_geometry(x$study_area) <- sf::st_centroid(sf::st_geometry(x$study_area))
  expect_error(do.call(terrain_development_summary, x), "polygon sf")
})

test_that("DEM rectangle is distinct from valid coverage and Study Area geometry", {
  d <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:26914")
  terra::values(d) <- c(1, NA, 3, 4)
  before <- terra::values(d)
  s <- terrain_development_summary(dem = d)
  expect_null(s$study_area)
  expect_equal(as.numeric(sf::st_area(s$dem_extent)), 4)
  expect_true(any(grepl("internal NoData", s$terrain$value)))
  expect_identical(terra::values(d), before)
  expect_error(terrain_development_summary(dem = c(d,d)), "single-band")
})

test_that("network GeoPackage reporting preserves history and does not accept", {
  x <- terrain_test_context()
  conf <- create_stream_network_configuration(.fg_generate_uuid(1), x$study_area$study_area_id,
    "Synthetic", "STREAM", x$streams, actor = "fixture")
  obs <- create_stream_network_observation(.fg_generate_uuid(1), conf$stream_network_configuration$stream_network_configuration_id,
    observation_year = 2006L, evidence_class = "SOURCE_NETWORK_RETAINED", coverage_status = "PARTIAL_CONFIGURATION",
    derivation_method_id = "SYNTHETIC", topology_tolerance = 0.01, topology_tolerance_unit = "METRE",
    native_horizontal_crs = "EPSG:26914", horizontal_unit = "METRE", provenance_completeness = "PARTIAL_LEGACY", actor = "fixture")
  line <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(50, 50))), crs = 26914))
  p <- prepare_stream_network_from_features(line, data.frame(source_row = 1L, stream_id = x$streams$stream_id),
    conf$stream_network_configuration, conf$stream_network_configuration_stream, obs, actor = "fixture")
  bundle <- c(conf, list(stream_network_observation = obs), p)
  before <- bundle
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path))
  write_stream_network_geodatabase(bundle, path)
  digest <- tools::md5sum(path)
  s <- terrain_development_summary(network = path)
  expect_identical(bundle, before)
  expect_identical(tools::md5sum(path), digest)
  expect_equal(s$observation$review_status, "DRAFT")
  expect_equal(s$segments$report_reach, "Synthetic Creek / Reach unassigned")
  expect_true(any(grepl("not accepted", s$gaps)))
  expect_equal(nrow(s$surveys), 0L)
  d <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:26914")
  expect_true(any(grepl("outside the supplied DEM", terrain_development_summary(network = bundle, dem = d)$gaps)))
  bundle$stream_network_observation$review_status <- "ACCEPTED"
  expect_true(any(grepl("Stored acceptance cannot be confirmed", terrain_development_summary(network = bundle)$gaps)))
})

test_that("HTML render is self-contained, escaped, and non-replacing", {
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc not available")
  x <- terrain_test_context()
  x$analyst_notes <- "<script>alert('unsafe')</script>"
  s <- do.call(terrain_development_summary, x)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  expect_invisible(terrain_development_report(s, path))
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>alert('unsafe')</script>", html, fixed = TRUE))
  expect_match(html, "2016-02-29", fixed = TRUE)
  expect_false(grepl('<(?:script|img)[^>]+src=["\x27]https?://', html, perl = TRUE))
  digest <- tools::md5sum(path)
  expect_error(terrain_development_report(s, path), "already exists")
  expect_identical(tools::md5sum(path), digest)
  sf::st_geometry(x$study_area) <- sf::st_transform(sf::st_geometry(x$study_area), 4326)
  stream_aoi <- sf::st_sf(x$streams, geometry = sf::st_geometry(x$study_area))
  only_aoi <- terrain_development_summary(streams = stream_aoi)
  aoi_path <- tempfile(fileext = ".html")
  on.exit(unlink(aoi_path), add = TRUE)
  terrain_development_report(only_aoi, aoi_path)
  expect_match(paste(readLines(aoi_path, warn = FALSE), collapse = "\n"), "data:image/png;base64,", fixed = TRUE)
})
