test_that("a chosen boundary preserves identity, child records and original evidence", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  aid <- .fg_generate_uuid(1); sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1)
  f <- list(study_area = data.frame(study_area_id = aid, study_area_name = "Study"),
    streams = data.frame(stream_id = sid, study_area_id = aid, stream_name = "Creek"),
    reaches = data.frame(reach_id = rid, stream_id = sid, reach_name = "R1"),
    survey_events = data.frame(survey_event_id = .fg_generate_uuid(2), reach_id = rid,
      survey_year = c(2006L, 2010L)), analyst_notes = "Prior scope notes")
  source <- do.call(write_study_context, c(list(dsn = file.path(root, "source.gpkg")), f))
  before <- tools::md5sum(source)
  boundary <- sf::st_sf(unrelated_name = "Never import this name", geometry = sf::st_as_sfc(
    sf::st_bbox(c(xmin = 720000, ymin = 4560000, xmax = 721000, ymax = 4561000), crs = 26914)))
  output <- file.path(root, "boundary.gpkg")
  revise_study_context(source, output, add_note = "Chosen scope, not approval.",
    study_area_boundary = boundary)
  x <- read_study_context(output); original <- read_study_context(source)
  expect_identical(sf::st_drop_geometry(x$study_area), original$study_area)
  expect_identical(sf::st_as_binary(sf::st_geometry(x$study_area)), sf::st_as_binary(sf::st_geometry(boundary)))
  expect_true(sf::st_crs(x$study_area) == sf::st_crs(boundary))
  expect_identical(x$analyst_notes, paste(original$analyst_notes,
    "Study Area boundary supplied: Chosen scope, not approval.", sep = "\n\n"))
  x$study_area <- original$study_area; x$analyst_notes <- original$analyst_notes
  expect_equal(x, original)
  expect_identical(tools::md5sum(source), before)
  # A replacement retains identity and records that this was a replacement.
  multi <- sf::st_cast(boundary, "MULTIPOLYGON")
  revised <- file.path(root, "replaced.gpkg")
  revise_study_context(output, revised, add_note = "Multipart source.", study_area_boundary = multi)
  y <- read_study_context(revised)
  expect_identical(y$study_area$study_area_id, original$study_area$study_area_id)
  expect_match(y$analyst_notes, "Study Area boundary replaced: Multipart source.", fixed = TRUE)
  expect_identical(sf::st_as_binary(sf::st_geometry(y$study_area)), sf::st_as_binary(sf::st_geometry(multi)))
})

test_that("ambiguous or invalid boundaries fail before publication", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  source <- start_study_context(file.path(root, "source.gpkg"), "Study")$context
  output <- file.path(root, "never.gpkg")
  shape <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 2, ymax = 2), crs = 26914))
  boundary <- sf::st_sf(geometry = shape)
  unknown <- suppressWarnings(sf::st_set_crs(boundary, NA))
  invalid <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
    c(0,0, 2,2, 0,2, 2,0, 0,0), ncol = 2, byrow = TRUE))), crs = 26914))
  xyz <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
    c(0,0,1, 2,0,1, 2,2,1, 0,0,1), ncol = 3, byrow = TRUE))), crs = 26914))
  bads <- list(boundary[FALSE, ], rbind(boundary, boundary), unknown, invalid, xyz,
    sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(), crs = 26914)),
    sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 26914)), data.frame(x = 1))
  before <- tools::md5sum(source)
  for (bad in bads) {
    expect_error(revise_study_context(source, output, add_note = "Reason", study_area_boundary = bad),
      "boundary feature|Boundary must")
    expect_false(file.exists(output))
  }
  expect_error(revise_study_context(source, output, study_area_boundary = boundary), "source and rationale")
  expect_error(revise_study_context(source, output, add_note = " ", study_area_boundary = boundary), "add_note")
  empty <- write_study_context(file.path(root, "empty.gpkg"))
  expect_error(revise_study_context(empty, output, add_note = "Reason", study_area_boundary = boundary), "No supplied Study Area")
  expect_false(file.exists(output))
  expect_identical(tools::md5sum(source), before)
})

test_that("the definition report maps a saved boundary and retains open hierarchy decisions", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  source <- start_study_context(file.path(root, "source.gpkg"), "Study")$context
  boundary <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = 720000, ymin = 4560000, xmax = 721000, ymax = 4561000), crs = 26914)))
  report <- file.path(root, "report.html")
  revise_study_context(source, file.path(root, "bound.gpkg"), add_note = "Test scope",
    study_area_boundary = boundary, report_file = report, report_purpose = "definition")
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "Working Study Area extent", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  expect_match(html, "Choose the Streams to include", fixed = TRUE)
  expect_false(grepl("Discuss the candidate Study Area extent", html, fixed = TRUE))
})
