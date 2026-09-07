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
  h <- s$hierarchy
  expect_equal(h$parent_id[h$entity_type == "Configuration"], x$study_area$study_area_id)
  expect_equal(h$parent_id[h$entity_type == "Observation"], conf$stream_network_configuration$stream_network_configuration_id)
  expect_false(any(h$entity_type == "Survey Event"))
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

terrain_test_reconstruction <- function() data.frame(case_id = "archive-1",
  source_ref = "unclassified/project.gdb", proposed_structure = "Possible parent Stream / R1",
  evidence = "A surviving filename; insufficient to assign hierarchy.", status = "PROPOSED",
  analyst = NA_character_, decision_notes = NA_character_)

test_that("forensic interpretation does not require or create governed identities", {
  cases <- terrain_test_reconstruction()
  before <- cases
  s <- terrain_development_summary(reconstruction = cases)
  expect_identical(cases, before)
  expect_identical(s$reconstruction, cases)
  expect_equal(nrow(s$hierarchy), 0L)
  expect_null(s$study_area)
  expect_null(s$streams)
  expect_true(s$assessment$requires_input[s$assessment$code == "ARCHIVE_INTERPRETATION"])
  cases$status <- "CONFIRMED"
  expect_error(terrain_development_summary(reconstruction = cases), "analyst and decision_notes")
  cases$analyst <- "fixture analyst"; cases$decision_notes <- "Confirmed from project scope document."
  confirmed <- terrain_development_summary(reconstruction = cases)
  expect_equal(nrow(confirmed$hierarchy), 0L)
  expect_false(confirmed$assessment$requires_input[confirmed$assessment$code == "ARCHIVE_INTERPRETATION"])
  cases$status <- "UNKNOWN"; cases$proposed_structure <- NA_character_
  expect_equal(terrain_development_summary(reconstruction = cases)$reconstruction$status, "UNKNOWN")
  cases$status <- "ACCEPTED"
  expect_error(terrain_development_summary(reconstruction = cases), "statuses")
  expect_error(terrain_development_summary(reconstruction = rbind(before, before)), "unique")
  cases <- before; cases$status <- "REJECTED"
  expect_error(terrain_development_summary(reconstruction = cases), "analyst and decision_notes")
})

test_that("named Study Area and identity-based hierarchy need no invented polygon", {
  x <- terrain_test_context()
  x$study_area <- sf::st_drop_geometry(x$study_area)
  s <- do.call(terrain_development_summary, x)
  expect_s3_class(s$study_area, "data.frame")
  expect_false(inherits(s$study_area, "sf"))
  expect_equal(nrow(s$hierarchy), 6L)
  expect_equal(s$hierarchy$entity_type, c("Study Area", "Stream", "Reach", rep("Survey Event", 3)))
  expect_equal(s$hierarchy$parent_id[4:6], rep(x$reaches$reach_id, 3))
  expect_true(any(s$assessment$code == "STUDY_AOI_NOT_SUPPLIED"))
  # Same labels and years are legal; neither is a join key.
  x$reaches <- rbind(x$reaches, transform(x$reaches, reach_id = .fg_generate_uuid(1)))
  x$survey_events <- rbind(x$survey_events, transform(x$survey_events,
    survey_event_id = .fg_generate_uuid(3), reach_id = x$reaches$reach_id[2]))
  s <- do.call(terrain_development_summary, x)
  expect_equal(nrow(s$event_evidence), 6L)
  expect_equal(length(unique(s$event_evidence$reach_id)), 2L)
  expect_equal(length(unique(s$hierarchy$node_key)), nrow(s$hierarchy))
})

test_that("per-event DEM metadata does not infer availability or comparability", {
  x <- terrain_test_context()
  d <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:26914")
  terra::values(d) <- c(1, NA, 2, 3)
  before <- terra::values(d)
  x$survey_dems <- setNames(list(d), x$survey_events$survey_event_id[2])
  s <- do.call(terrain_development_summary, x)
  expect_equal(s$event_evidence$evidence_status, c("INVENTORY_ONLY", "GRID_SUPPLIED", "INVENTORY_ONLY"))
  expect_equal(s$survey_dem_extents$survey_event_id, x$survey_events$survey_event_id[2])
  expect_identical(terra::values(d), before)
  expect_true(all(s$assessment$status[s$assessment$code == "EVENT_TERRAIN_REVIEW"] == "NOT_ASSESSED"))
  expect_equal(s$event_evidence$rows, c(NA_integer_, 2L, NA_integer_))
  x$survey_dems <- list(d)
  expect_error(do.call(terrain_development_summary, x), "uniquely named")
  x$survey_dems <- setNames(list(d), .fg_generate_uuid(1))
  expect_error(do.call(terrain_development_summary, x), "uniquely named")
  x$survey_dems <- setNames(list(c(d,d)), x$survey_events$survey_event_id[1])
  expect_error(do.call(terrain_development_summary, x), "single-band")
})

test_that("visual report handles archive-only and named-AOI-missing inputs safely", {
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc not available")
  cases <- terrain_test_reconstruction()
  cases$evidence <- "<script>untrusted archive text</script>"
  cases$proposed_structure <- "<b>Not markup</b>"
  x <- terrain_test_context()
  x$study_area <- sf::st_drop_geometry(x$study_area)
  x$reconstruction <- cases
  d <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:26914")
  x$survey_dems <- setNames(list(d), x$survey_events$survey_event_id[1])
  s <- do.call(terrain_development_summary, x)
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  terrain_development_report(s, path)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Study structure", fixed = TRUE)
  expect_match(html, "Legacy project reconstruction", fixed = TRUE)
  expect_match(html, "&lt;script&gt;untrusted", fixed = TRUE)
  expect_false(grepl("<b>Not markup</b>", html, fixed = TRUE))
  expect_gte(lengths(regmatches(html, gregexpr("data:image/png;base64,", html, fixed = TRUE))), 3L)
  archive_path <- tempfile(fileext = ".html")
  on.exit(unlink(archive_path), add = TRUE)
  expect_invisible(terrain_development_report(terrain_development_summary(reconstruction = cases), archive_path))
  old <- s; old$schema <- "TERRAIN_DEVELOPMENT_REPORT_1"
  old[c("hierarchy", "event_evidence", "survey_dem_extents", "reconstruction", "assessment")] <- NULL
  old_path <- tempfile(fileext = ".html")
  on.exit(unlink(old_path), add = TRUE)
  expect_invisible(terrain_development_report(old, old_path))
  expect_error(terrain_development_report(list(), tempfile(fileext = ".html")), "summary")
})
