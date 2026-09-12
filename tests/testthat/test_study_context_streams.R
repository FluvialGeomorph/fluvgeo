test_that("initial named Streams need no areas or acquisition and retain the study", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  src <- start_study_context(file.path(root, "draft.gpkg"), "Study", "Prior note")$context
  hash <- tools::md5sum(src)
  result <- define_study_streams(src, file.path(root, "streams.gpkg"),
    data.frame(Name = c(" Cole Creek ", "Other Creek"), ignored = c(1, 2)), "Name", "Customer scope")
  x <- read_study_context(result$context); before <- read_study_context(src)
  expect_identical(x$study_area, before$study_area)
  expect_identical(x$streams$stream_name, c("Cole Creek", "Other Creek"))
  expect_identical(x$streams$study_area_id, rep(before$study_area$study_area_id, 2))
  expect_length(unique(x$streams$stream_id), 2L)
  expect_match(x$streams$stream_id, "^[0-9a-f-]{36}$")
  expect_false(inherits(x$streams, "sf"))
  expect_null(x$reaches); expect_null(x$survey_events)
  expect_identical(x$analyst_notes, "Prior note\n\nInitial Streams defined (2): Customer scope")
  expect_identical(read_study_context(result$context)$streams, x$streams)
  expect_identical(tools::md5sum(src), hash)
  expect_error(define_study_streams(result$context, file.path(root, "again.gpkg"),
    data.frame(stream_name = "Third"), add_note = "No"), "already exist")
  expect_false(file.exists(file.path(root, "again.gpkg")))
})

test_that("explicit Stream polygons retain coordinates, names and native CRS", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  src <- start_study_context(file.path(root, "draft.gpkg"), "Study")$context
  areas <- sf::st_sf(Name = c("One", "Two"), geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,0), ncol=2, byrow=TRUE))),
    sf::st_polygon(list(matrix(c(2,0,3,0,3,1,2,0), ncol=2, byrow=TRUE))), crs=26914))
  result <- define_study_streams(src, file.path(root, "areas.gpkg"), areas, "Name", "Chosen areas",
    report_file = file.path(root, "report.html"))
  x <- read_study_context(result$context)
  expect_identical(sf::st_as_binary(sf::st_geometry(x$streams)), sf::st_as_binary(sf::st_geometry(areas)))
  expect_true(sf::st_crs(x$streams) == sf::st_crs(areas))
  expect_false(inherits(x$study_area, "sf"))
  html <- paste(readLines(result$report, warn=FALSE), collapse="\n")
  expect_match(html, "Selected Stream areas", fixed=TRUE)
  expect_match(html, "data:image/png;base64", fixed=TRUE)
  expect_match(html, "Agree on meaningful Reach divisions", fixed=TRUE)
  expect_false(grepl("Choose the Streams to include", html, fixed=TRUE))
  unknown <- suppressWarnings(sf::st_set_crs(areas, NA))
  expect_error(define_study_streams(src, file.path(root, "bad.gpkg"), unknown, "Name", "No"), "polygon sf with a CRS")
  expect_false(file.exists(file.path(root, "bad.gpkg")))
})

test_that("invalid initial inventories and destination collisions do not publish", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  src <- start_study_context(file.path(root, "draft.gpkg"), "Study")$context
  out <- file.path(root, "never.gpkg")
  for (values in list(character(), c("One", " one "), NA_character_, " ", 1L)) {
    expect_error(define_study_streams(src, out, data.frame(stream_name=values), add_note="Reason"),
      "Stream rows|Stream names|Duplicate")
    expect_false(file.exists(out))
  }
  rows <- data.frame(stream_name="One")
  expect_error(define_study_streams(src, out, rows, "absent", "Reason"), "name_column")
  expect_error(define_study_streams(src, out, rows, add_note=" "), "add_note")
  expect_error(define_study_streams(src, src, rows, add_note="Reason"), "already exists")
  report <- file.path(root, "old.html"); writeLines("keep", report)
  expect_error(define_study_streams(src, out, rows, add_note="Reason", report_file=report), "already exists")
  expect_identical(readLines(report), "keep")
  expect_false(file.exists(out))
  empty <- write_study_context(file.path(root, "empty.gpkg"))
  expect_error(define_study_streams(empty, out, rows, add_note="Reason"), "existing Study Area identity")
})
