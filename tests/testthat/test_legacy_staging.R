legacy_test_root <- function() {
  root <- tempfile("legacy-staging-")
  dir.create(root)
  root
}

legacy_test_listing <- function(names = "flowline") data.frame(layer = names,
  geometry_type = rep("3D Measured Line String", length(names)),
  features = rep(NA_real_, length(names)), fields = rep(3L, length(names)),
  crs = rep("NAD83 / UTM zone 15N", length(names)))

test_that("incomplete staging returns findings without inventing hierarchy", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  expect_error(inspect_legacy_staging(file.path(root, "absent")), "existing directory")
  empty <- inspect_legacy_staging(root)
  expect_equal(nrow(empty$geodatabases), 0L)
  expect_true(all(c("STAGING_NO_GDB", "STAGING_STUDY_CATALOG_MISSING",
    "STAGING_STREAM_FOLDERS_MISSING") %in% empty$assessment$code))
  expect_identical(empty$conversion_readiness, "NOT_ASSESSED")
  expect_identical(empty$fgdb_readiness, "NOT_ASSESSED")
  dir.create(file.path(root, "source.gdb"))
  expect_error(inspect_legacy_staging(file.path(root, "source.gdb")), "not a geodatabase")
})

test_that("legacy and explicit event placements are locators, not events or dates", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  paths <- c("Streams/Creek/Old_Stream.gdb", "Streams/Creek/Reaches/R01/CC_R1.gdb",
    "Streams/Creek/Reaches/R02/SurveyEvents/2006-a/data.gdb",
    "Streams/Creek/Reaches/R02/SurveyEvents/2006-b/data.gdb")
  for (p in paths) dir.create(file.path(root, p), recursive = TRUE)
  local_mocked_bindings(.fg_legacy_layers = function(path) legacy_test_listing())
  x <- inspect_legacy_staging(root)
  expect_setequal(x$geodatabases$path, paths)
  expect_equal(sum(x$geodatabases$placement == "EVENT_LOCATION"), 2L)
  expect_equal(sum(x$assessment$code == "STAGING_EVENT_FOLDER_MISSING"), 1L)
  expect_true("STAGING_STREAM_CATALOG_MISSING" %in% x$assessment$code)
  expect_false(any(c("survey_year", "survey_event_id", "reach_id") %in% names(x$geodatabases)))
  expect_true(all(is.na(x$layers$features)))
  expect_equal(x$layers$geometry_type, rep("3D Measured Line String", 4))
  s <- terrain_development_summary(legacy_staging = root)
  expect_equal(nrow(s$hierarchy), 0L)
  expect_equal(nrow(s$surveys), 0L)
  expect_equal(nrow(s$staging_inventory$geodatabases), 4L)
  expect_true("LEGACY_STAGING" %in% s$review_actions$stage)
})

test_that("catalog presence never certifies values, dates or readiness", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "StudyArea.gdb"))
  dir.create(file.path(root, "Streams/Creek/Stream.gdb"), recursive = TRUE)
  local_mocked_bindings(.fg_legacy_layers = function(path) {
    if (basename(path) == "StudyArea.gdb") legacy_test_listing(c("study_area",
      "study_area_geometry", "stream_catalog", "migration_source", "migration_item"))
    else legacy_test_listing(c("stream", "reach", "survey_event", "event_source"))
  })
  x <- inspect_legacy_staging(root)
  expect_true(all(x$catalogs$present))
  expect_false(any(grepl("CATALOG_MISSING", x$assessment$code)))
  expect_true("STAGING_VALIDATION_LIMIT" %in% x$assessment$code)
  expect_identical(x$conversion_readiness, "NOT_ASSESSED")
})

test_that("failed and warned listings retain uncertainty and other sources", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  for (p in c("StudyArea.gdb", "warn.gdb", "ok.gdb")) dir.create(file.path(root, p))
  local_mocked_bindings(.fg_legacy_layers = function(path) {
    if (basename(path) == "StudyArea.gdb") stop("fixture read failure")
    if (basename(path) == "warn.gdb") warning("fixture driver warning")
    legacy_test_listing()
  })
  x <- inspect_legacy_staging(root)
  expect_true(all(is.na(x$catalogs$present)))
  expect_false("STAGING_STUDY_CATALOG_MISSING" %in% x$assessment$code)
  expect_setequal(x$geodatabases$listing_status, c("LISTED", "LISTED_WITH_WARNINGS", "UNREADABLE"))
  expect_equal(nrow(x$layers), 2L)
  expect_true(all(c("STAGING_GDB_UNREADABLE", "STAGING_DRIVER_WARNING") %in% x$assessment$code))
})

test_that("traversal skips links and excessive nesting and stops at geodatabases", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "outer.gdb/hidden.gdb"), recursive = TRUE)
  deep <- paste(rep("nested", 9), collapse = "/")
  dir.create(file.path(root, deep, "deep.gdb"), recursive = TRUE)
  local_mocked_bindings(.fg_legacy_layers = function(path) legacy_test_listing())
  x <- inspect_legacy_staging(root)
  expect_equal(x$geodatabases$path, "outer.gdb")
  expect_true("STAGING_DEPTH_LIMIT" %in% x$assessment$code)
})

test_that("linked directories are not followed", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  outside <- legacy_test_root(); on.exit(unlink(outside, recursive = TRUE), add = TRUE)
  dir.create(file.path(outside, "outside.gdb"))
  linked <- suppressWarnings(file.symlink(outside, file.path(root, "link")))
  skip_if_not(linked, "Directory symlinks unavailable to this account")
  x <- inspect_legacy_staging(root)
  expect_equal(nrow(x$geodatabases), 0L)
  expect_true("STAGING_PATH_SKIPPED" %in% x$assessment$code)
})

test_that("unreadable directories are reported, not treated as empty proof", {
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  local_mocked_bindings(.fg_legacy_children = function(path) stop("fixture inaccessible"))
  x <- inspect_legacy_staging(root)
  expect_true("STAGING_DIRECTORY_UNREADABLE" %in% x$assessment$code)
  expect_false(any(grepl("CATALOG_MISSING", x$assessment$code)))
  expect_true(all(is.na(x$catalogs$present)))
  expect_false(x$directory_inventory_complete)
  expect_identical(x$conversion_readiness, "NOT_ASSESSED")
})

test_that("real OpenFileGDB metadata inspection preserves source bytes", {
  skip_if_not("OpenFileGDB" %in% sf::st_drivers()$name)
  skip_if_not(sf::st_drivers()$write[sf::st_drivers()$name == "OpenFileGDB"])
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  gdb <- file.path(root, "fixture.gdb")
  feature <- sf::st_sf(label = "fixture", geometry = sf::st_sfc(sf::st_point(c(1, 2, 3)), crs = 26915))
  sf::st_write(feature, gdb, layer = "point_z", driver = "OpenFileGDB", quiet = TRUE)
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  before <- tools::md5sum(files)
  x <- inspect_legacy_staging(root)
  expect_identical(list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE), files)
  expect_identical(tools::md5sum(files), before)
  expect_equal(x$layers$layer, "point_z")
  expect_match(x$layers$geometry_type, "3D")
  expect_equal(x$geodatabases$listing_status, "LISTED")
})

test_that("legacy inventory renders escaped within the existing report", {
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc unavailable")
  root <- legacy_test_root(); on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "fixture.gdb"))
  local_mocked_bindings(.fg_legacy_layers = function(path) legacy_test_listing("<unsafe>"))
  s <- terrain_development_summary(legacy_staging = root)
  dest <- tempfile(fileext = ".html"); on.exit(unlink(dest), add = TRUE)
  terrain_development_report(s, dest)
  html <- paste(readLines(dest, warn = FALSE), collapse = "\n")
  expect_match(html, "FileGDB staging inventory", fixed = TRUE)
  expect_match(html, "&lt;unsafe&gt;", fixed = TRUE)
  expect_true(grepl("not[[:space:]]+confirmed Survey Events", html))
  expect_false(grepl("<unsafe>", html, fixed = TRUE))
})

test_that("Staging Report separates reconstruction from terrain assessment", {
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc unavailable")
  s <- terrain_development_summary()
  terrain_row <- s$assessment[1, , drop = FALSE]
  terrain_row$stage <- "TERRAIN_REVIEW"
  terrain_row$code <- "TERRAIN_ONLY_SENTINEL"
  terrain_row$next_action <- "TERRAIN_ONLY_SENTINEL"
  s$assessment <- rbind(s$assessment, terrain_row)
  grouped <- .fg_terrain_review_actions(s$assessment)
  s$review_actions <- grouped$review_actions
  s$review_action_members <- grouped$review_action_members
  before <- s
  dest <- tempfile(fileext = ".html"); on.exit(unlink(dest))
  expect_invisible(study_staging_report(s, dest))
  expect_identical(s, before)
  html <- paste(readLines(dest, warn = FALSE), collapse = "\n")
  expect_true(grepl("Study Area Staging Report", html, fixed = TRUE))
  expect_false(grepl("TERRAIN_ONLY_SENTINEL", html, fixed = TRUE))
  expect_false(grepl("Survey terrain extents", html, fixed = TRUE))
  expect_true(grepl("What the analyst needs to do next", html, fixed = TRUE))
  digest <- tools::md5sum(dest)
  expect_error(study_staging_report(s, dest), "already exists")
  expect_identical(tools::md5sum(dest), digest)
})
