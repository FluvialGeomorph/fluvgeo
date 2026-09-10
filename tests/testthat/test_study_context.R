study_context_fixture <- function() {
  aid <- .fg_generate_uuid(1); sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1)
  list(study_area = data.frame(study_area_id = aid, study_area_name = "Study <draft>"),
    streams = data.frame(stream_id = sid, study_area_id = aid, stream_name = "Creek"),
    reaches = data.frame(reach_id = rid, stream_id = sid, reach_name = "R1"),
    survey_events = data.frame(survey_event_id = .fg_generate_uuid(2), reach_id = rid,
      survey_year = c(2006L, 2006L), survey_month = c(NA_integer_, 4L),
      survey_day = c(NA_integer_, 2L), source_dataset = c("legacy.gdb", NA_character_)),
    analyst_notes = "Provisional identities.\n\nNo acceptance implied.")
}

test_that("revision changes only the supplied name and appended note", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  f <- study_context_fixture()
  f$study_area <- sf::st_sf(f$study_area, geometry = sf::st_as_sfc(
    sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 2, ymax = 2), crs = 26914)))
  source <- do.call(write_study_context, c(list(dsn = file.path(root, "original.gpkg")), f))
  before <- tools::md5sum(source)
  target <- file.path(root, "revised.gpkg")
  result <- revise_study_context(source, target, "New <name>", "Analyst's scope note")
  expect_equal(result, list(context = normalizePath(target, winslash = "/"), report = NULL))
  original <- read_study_context(source); edited <- read_study_context(target)
  expect_identical(edited$study_area$study_area_name, "New <name>")
  expect_identical(edited$analyst_notes, paste(original$analyst_notes, "Analyst's scope note", sep = "\n\n"))
  edited$study_area$study_area_name <- original$study_area$study_area_name
  edited$analyst_notes <- original$analyst_notes
  expect_equal(edited, original)
  expect_identical(tools::md5sum(source), before)
  expect_error(revise_study_context(source, target, add_note = "again"), "already exists")
  fresh <- file.path(root, "not-written.gpkg")
  expect_error(revise_study_context(source, fresh), "No changes")
  expect_error(revise_study_context(source, fresh, f$study_area$study_area_name), "No changes")
  expect_error(revise_study_context(source, fresh, " "), "study_area_name")
  expect_error(revise_study_context(source, fresh, add_note = " "), "add_note")
  dir.create(file.path(root, "sub"))
  expect_error(revise_study_context(source, file.path(root, "sub", "new.gpkg"), "X"), "beside")
  expect_error(revise_study_context(source, fresh, "X", report_file = source), "html")
  report <- file.path(root, "report.html"); writeLines("existing", report)
  expect_error(revise_study_context(source, fresh, "X", report_file = report), "already exists")
  expect_identical(readLines(report), "existing")
  expect_false(file.exists(fresh))
})

test_that("revision cannot invent context and explains retained partial output", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  source <- write_study_context(file.path(root, "empty.gpkg"))
  target <- file.path(root, "revised.gpkg")
  expect_error(revise_study_context(source, target, "Invented"), "No supplied Study Area")
  testthat::local_mocked_bindings(terrain_development_report = function(...) stop("Render probe"))
  expect_error(revise_study_context(source, target, add_note = "Unresolved archive",
    report_file = file.path(root, "report.html")), "Revised context saved.*report generation failed.*Render probe")
  expect_true(file.exists(target))
  expect_null(read_study_context(target)$study_area)
  expect_identical(read_study_context(target)$analyst_notes, "Unresolved archive")
  expect_false(file.exists(file.path(root, "report.html")))
})

test_that("context round trip retains supplied hierarchy, dates, geometry and notes", {
  root <- tempfile("study-context-"); dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  f <- study_context_fixture()
  polygon <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 2, ymax = 2), crs = 26914))
  f$study_area <- sf::st_sf(f$study_area, boundary = polygon)
  path <- do.call(write_study_context, c(list(dsn = file.path(root, "study.gpkg")), f))
  before <- tools::md5sum(path)
  x <- read_study_context(path)
  expect_equal(sf::st_drop_geometry(x$study_area), sf::st_drop_geometry(f$study_area))
  expect_true(sf::st_crs(x$study_area) == sf::st_crs(f$study_area))
  expect_identical(sf::st_as_binary(sf::st_geometry(x$study_area)), sf::st_as_binary(polygon))
  expect_identical(attr(x$study_area, "sf_column"), "boundary")
  for (nm in c("streams", "reaches", "survey_events", "analyst_notes")) expect_equal(x[[nm]], f[[nm]])
  direct <- do.call(terrain_development_summary, f)
  fields <- setdiff(names(direct), c("generated_at", "study_area"))
  expect_equal(read_study_context_summary(path)[fields], direct[fields])
  expect_identical(tools::md5sum(path), before)
  expect_error(do.call(write_study_context, c(list(dsn = path), f)), "already exists")
  expect_identical(tools::md5sum(path), before)
  moved <- file.path(root, "moved"); dir.create(moved)
  expect_true(file.copy(path, moved))
  expect_equal(read_study_context_summary(file.path(moved, "study.gpkg"))[fields], direct[fields])
})

test_that("empty and forensic-only contexts never fabricate hierarchy", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  path <- write_study_context(file.path(root, "empty.gpkg"))
  expect_null(read_study_context_summary(path)$study_area)
  ledger <- data.frame(case_id = "unknown", source_ref = "archive", proposed_structure = NA_character_,
    evidence = "No parent record", status = "UNKNOWN", analyst = NA_character_, decision_notes = NA_character_)
  p <- write_study_context(file.path(root, "forensic.gpkg"), reconstruction = ledger)
  expect_equal(read_study_context(p)$reconstruction, ledger)
  expect_null(read_study_context_summary(p)$study_area)
  expect_true("ARCHIVE_INTERPRETATION" %in% read_study_context_summary(p)$assessment$code)
})

test_that("linked terrain is portable and fresh failures remain visible", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  f <- study_context_fixture()
  dem <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:26914")
  terra::values(dem) <- c(1, 0, NA, -1)
  terra::writeRaster(dem, file.path(root, "dem.tif"))
  links <- data.frame(artifact_id = "dem", survey_event_id = f$survey_events$survey_event_id[1],
    purpose = "test", evidence = "synthetic", analyst = "test", use_for_report = TRUE)
  write_terrain_manifest(root, data.frame(artifact_id = "dem", path = "dem.tif", role = "terrain"),
    "test", event_links = links)
  path <- do.call(write_study_context, c(list(dsn = file.path(root, "study.gpkg"), folder_manifest = "terrain-manifest.json"), f))
  x <- read_study_context_summary(path)
  revised <- revise_study_context(path, file.path(root, "revised.gpkg"), add_note = "Review note")
  expect_equal(read_study_context_summary(revised$context)$event_artifacts, x$event_artifacts)
  expect_equal(read_study_context(path)$folder_manifest, read_study_context(revised$context)$folder_manifest)
  expect_equal(x$event_evidence$evidence_status, c("GRID_SUPPLIED", "INVENTORY_ONLY"))
  expect_false(any(grepl("grid metadata are unavailable", x$gaps)))
  expect_true("VERTICAL_REFERENCE_UNKNOWN" %in% x$assessment$code)
  moved <- file.path(root, "moved"); dir.create(moved)
  expect_true(all(file.copy(list.files(root, full.names = TRUE, pattern = "\\.(gpkg|tif|json)$"), moved)))
  p <- file.path(moved, "study.gpkg")
  expect_equal(read_study_context_summary(p)$event_artifacts, x$event_artifacts)
  unlink(file.path(moved, "dem.tif"))
  y <- read_study_context_summary(p)
  expect_true("FILE_MISSING" %in% y$assessment$code)
  expect_equal(y$event_artifacts$grid_status, "NOT_LOADED")
  cat("\n", file = file.path(moved, "terrain-manifest.json"), append = TRUE)
  expect_error(read_study_context(p), "changed")
  unlink(file.path(moved, "terrain-manifest.json"))
  expect_error(read_study_context(p), "missing")
})

test_that("invalid and unsupported context fails before publication", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  path <- file.path(root, "bad.gpkg"); f <- study_context_fixture()
  bad <- f; bad$reaches$stream_id <- .fg_generate_uuid(1)
  expect_error(do.call(write_study_context, c(list(dsn = path), bad)), "parent Streams")
  bad <- f; bad$streams$extra <- "unsupported"
  expect_error(do.call(write_study_context, c(list(dsn = path), bad)), "Unsupported context columns")
  bad <- f; bad$survey_events$survey_year <- as.double(bad$survey_events$survey_year)
  expect_error(do.call(write_study_context, c(list(dsn = path), bad)), "integer")
  expect_error(write_study_context(path, network = "../outside.gpkg"), "Unsafe relative path")
  expect_error(write_study_context(path, network = "C:/outside.gpkg"), "Unsafe relative path")
  expect_false(file.exists(path))
  sf::st_write(f$streams, path, layer = "ordinary", quiet = TRUE)
  expect_error(read_study_context(path), "Missing Study Area context binding")
  p <- write_study_context(file.path(root, "version.gpkg"))
  meta <- sf::st_read(p, layer = "fluvgeo_study_context", quiet = TRUE)
  meta$schema <- "FUTURE"
  sf::st_write(meta, p, layer = "fluvgeo_study_context", delete_layer = TRUE, quiet = TRUE)
  expect_error(read_study_context(p), "Unsupported or malformed")
})
