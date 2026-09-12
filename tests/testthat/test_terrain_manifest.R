manifest_fixture <- function() {
  root <- tempfile("intake-"); dir.create(root)
  d <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3, crs = "EPSG:26914")
  terra::values(d) <- c(0, -1, .125, NA, 5, 6, 7, 8, 9)
  terra::writeRaster(d, file.path(root, "dem.tif"), datatype = "FLT4S")
  sf::st_write(sf::st_sf(label = "fixture", geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = 26914)),
               file.path(root, "vectors.gpkg"), layer = "points", quiet = TRUE)
  list(root = root, artifacts = data.frame(artifact_id = c("terrain-1", "vectors-1"),
       path = c("dem.tif", "vectors.gpkg"), role = c("retained-dem", "context-vectors")))
}

test_that("portable manifest survives relocation without modifying sources", {
  f <- manifest_fixture()
  before <- tools::md5sum(file.path(f$root, f$artifacts$path))
  m <- write_terrain_manifest(f$root, f$artifacts, "unreconciled-case")
  x <- inspect_terrain_folder(m)
  expect_true(all(x$artifacts$hash_verified))
  expect_true("VERTICAL_REFERENCE_UNKNOWN" %in% x$assessment$code)
  expect_false(any(x$assessment$status == "BLOCKED"))
  expect_identical(tools::md5sum(names(before)), before)
  text <- paste(readLines(m), collapse = "")
  expect_false(grepl(normalizePath(f$root, winslash = "/"), text, fixed = TRUE))
  moved <- tempfile("relocated folder "); dir.create(moved)
  expect_true(all(file.copy(list.files(f$root, full.names = TRUE), moved)))
  y <- inspect_terrain_folder(file.path(moved, basename(m)))
  expect_identical(x, y)
  expect_error(write_terrain_manifest(f$root, f$artifacts, "case"), "already exists")
  s <- terrain_development_summary(folder_manifest = m)
  expect_equal(s$folder_inventory$intake_id, "unreconciled-case")
  expect_true("FILE_HASH_VERIFIED" %in% s$assessment$code)
  expect_null(s$study_area)
  expect_equal(nrow(s$surveys), 0L)
})

test_that("changed and missing files remain visible without rewriting evidence", {
  f <- manifest_fixture(); m <- write_terrain_manifest(f$root, f$artifacts, "case")
  original <- tools::md5sum(m)
  con <- file(file.path(f$root, "vectors.gpkg"), "ab"); writeBin(as.raw(0), con); close(con)
  expect_true("FILE_CHANGED" %in% inspect_terrain_folder(m)$assessment$code)
  file.rename(file.path(f$root, "dem.tif"), file.path(f$root, "held.tif"))
  x <- inspect_terrain_folder(m)
  expect_true("FILE_MISSING" %in% x$assessment$code)
  expect_false(x$artifacts$available[1])
  expect_identical(tools::md5sum(m), original)
})

test_that("relative paths and schemas fail closed", {
  f <- manifest_fixture()
  for (bad in c("../outside.tif", "/absolute.tif", "C:/outside.tif", "dir\\file.tif", "x/../dem.tif", "x//dem.tif")) {
    a <- f$artifacts; a$path[1] <- bad
    expect_error(write_terrain_manifest(f$root, a, "case"), "Unsafe")
  }
  a <- f$artifacts; a$artifact_id[2] <- a$artifact_id[1]
  expect_error(write_terrain_manifest(f$root, a, "case"), "unique")
  m <- write_terrain_manifest(f$root, f$artifacts, "case")
  x <- jsonlite::read_json(m); x$schema <- "FUTURE_UNKNOWN"
  bad <- file.path(f$root, "bad.json"); jsonlite::write_json(x, bad, auto_unbox = TRUE, null = "null")
  expect_error(inspect_terrain_folder(bad), "Unsupported")
  x$schema <- "FLUVGEO_TERRAIN_INTAKE_1"; x$artifacts[[1]]$path <- "../outside.tif"
  jsonlite::write_json(x, bad, auto_unbox = TRUE, null = "null")
  expect_error(inspect_terrain_folder(bad), "Unsafe")
})

test_that("known vertical metadata needs evidence and does not assign identities", {
  f <- manifest_fixture(); a <- f$artifacts
  a$vertical_unit <- c("m", NA_character_)
  a$vertical_reference <- c("synthetic local reference", NA_character_)
  expect_error(write_terrain_manifest(f$root, a, "case"), "requires metadata_evidence")
  a$metadata_evidence <- c("Synthetic test specification, not site metadata", NA_character_)
  x <- inspect_terrain_folder(write_terrain_manifest(f$root, a, "case"))
  expect_false("VERTICAL_REFERENCE_UNKNOWN" %in% x$assessment$code)
  expect_identical(x$artifacts$vertical_unit, c("m", NA_character_))
})

test_that("added and conflicting sidecars are not silently trusted", {
  f <- manifest_fixture(); m <- write_terrain_manifest(f$root, f$artifacts, "case")
  sidecar <- file.path(f$root, "dem.tif.aux.xml")
  writeLines(c('<PAMDataset>', '<GeoTransform>100,1,0,103,0,-1</GeoTransform>', '</PAMDataset>'), sidecar)
  x <- inspect_terrain_folder(m)
  expect_true("COMPANION_CHANGED" %in% x$assessment$code)
  expect_true("SIDECAR_METADATA_CONFLICT" %in% x$assessment$code)
  expect_true(all(x$artifacts$hash_verified))
  expect_true(any(x$assessment$status == "BLOCKED"))
})

test_that("renamed virtual rasters are rejected before opening", {
  f <- manifest_fixture()
  writeLines('<VRTDataset/>', file.path(f$root, "fake.tif"))
  a <- f$artifacts[1, ]; a$path <- "fake.tif"
  expect_error(write_terrain_manifest(f$root, a, "case"), "native TIFF")
})

test_that("declared units cannot silently contradict raster units", {
  f <- manifest_fixture()
  d <- terra::rast(file.path(f$root, "dem.tif"))
  terra::values(d) <- terra::values(d); terra::units(d) <- "m"
  terra::writeRaster(d, file.path(f$root, "with-units.tif"))
  a <- f$artifacts[1, ]; a$path <- "with-units.tif"
  a$vertical_unit <- "ft"; a$vertical_reference <- "synthetic"
  a$metadata_evidence <- "Deliberately contradictory test assertion"
  x <- inspect_terrain_folder(write_terrain_manifest(f$root, a, "case"))
  expect_true("VERTICAL_UNIT_CONFLICT" %in% x$assessment$code)
  a$vertical_unit <- "metres"
  x <- inspect_terrain_folder(write_terrain_manifest(f$root, a, "case", "synonym.json"))
  expect_false("VERTICAL_UNIT_CONFLICT" %in% x$assessment$code)
})

test_that("recorded companion hashes survive JSON and flag later edits", {
  f <- manifest_fixture()
  p <- file.path(f$root, "dem.prj"); writeLines("Unverified accompanying source note", p)
  m <- write_terrain_manifest(f$root, f$artifacts, "case")
  expect_false("COMPANION_CHANGED" %in% inspect_terrain_folder(m)$assessment$code)
  writeLines("Changed source note", p)
  expect_true("COMPANION_CHANGED" %in% inspect_terrain_folder(m)$assessment$code)
})

test_that("metadata comparison tolerates representation but not grid shifts", {
  f <- manifest_fixture()
  a <- .fg_manifest_observe(file.path(f$root, "dem.tif"))
  b <- a; b$extent <- b$extent + .Machine$double.eps
  expect_true(.fg_manifest_same_metadata(a, b))
  b$extent[1] <- b$extent[1] + .5
  expect_false(.fg_manifest_same_metadata(a, b))
})

test_that("the report presents intake results and escapes supplied labels", {
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available())
  f <- manifest_fixture()
  m <- write_terrain_manifest(f$root, f$artifacts, "<b>local case</b>")
  html <- file.path(f$root, "report.html")
  terrain_development_report(terrain_development_summary(folder_manifest = m), html)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_true(grepl("Portable intake files", text, fixed = TRUE))
  expect_true(grepl("Matches snapshot", text, fixed = TRUE))
  expect_true(grepl("&lt;b&gt;local case&lt;/b&gt;", text, fixed = TRUE))
  expect_false(grepl("<b>local case</b>", text, fixed = TRUE))
})
