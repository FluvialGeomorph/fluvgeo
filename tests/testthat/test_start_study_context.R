test_that("new studies start without acquired or inferred data", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  name <- "Creek \u2014 <draft>"
  notes <- "Customer's question\n\nCompare C:\\terrain\\new; \"options\"."
  first <- start_study_context(file.path(root, "draft.gpkg"), name, notes)
  args <- read_study_context(first$context)
  expect_identical(args$study_area$study_area_name, name)
  expect_identical(args$analyst_notes, notes)
  expect_identical(names(args), c("study_area", "terrain_notes", "analyst_notes"))
  expect_identical(args$terrain_notes, NA_character_)
  expect_false(inherits(args$study_area, "sf"))
  expect_match(args$study_area$study_area_id, .fg_uuid_pattern)
  expect_null(first$report)
  second <- start_study_context(file.path(root, "second.gpkg"), name)
  expect_false(identical(read_study_context(second$context)$study_area$study_area_id,
    args$study_area$study_area_id))
  expect_identical(read_study_context(second$context)$analyst_notes, NA_character_)
  before <- tools::md5sum(first$context)
  revision <- revise_study_context(first$context, file.path(root, "revision.gpkg"),
    add_note = "Customer scope discussion continues.")
  expect_identical(read_study_context(revision$context)$study_area, args$study_area)
  expect_identical(tools::md5sum(first$context), before)
  expect_equal(nrow(read_study_context_summary(first$context)$surveys), 0L)
})

test_that("starter refuses invalid inputs and occupied destinations before writing", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  path <- file.path(root, "draft.gpkg")
  for (name in list("", " ", NA_character_, c("A", "B"), 123)) {
    expect_error(start_study_context(path, name), "study_area_name")
    expect_false(file.exists(path))
  }
  for (notes in list(" ", c("A", "B"), 123)) {
    expect_error(start_study_context(path, "Study", notes), "analyst_notes")
    expect_false(file.exists(path))
  }
  report <- file.path(root, "report.html"); writeLines("existing", report)
  expect_error(start_study_context(path, "Study", report_file = report), "already exists")
  expect_identical(readLines(report), "existing")
  expect_false(file.exists(path))
  expect_error(start_study_context(path, "Study", report_file = path), "html")
  expect_false(file.exists(path))
  expect_error(start_study_context(file.path(root, "absent", "draft.gpkg"), "Study"))
  start_study_context(path, "Study")
  before <- tools::md5sum(path)
  expect_error(start_study_context(path, "Other"), "already exists")
  expect_identical(tools::md5sum(path), before)
})

test_that("report failure retains the draft with actionable recovery", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  testthat::local_mocked_bindings(define_study_area_report = function(...) stop("Render probe"))
  path <- file.path(root, "draft.gpkg"); report <- file.path(root, "report.html")
  expect_error(start_study_context(path, "Study", report_file = report), "Draft context saved.*retry")
  expect_identical(read_study_context(path)$study_area$study_area_name, "Study")
  expect_false(file.exists(report))
})

test_that("Define Study Area describes open design without legacy or terrain gates", {
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  result <- start_study_context(file.path(root, "draft.gpkg"), "A <draft>",
    analyst_notes = "<script>alert('test')</script>\nC:\\terrain\\new",
    report_file = file.path(root, "report.html"))
  html <- paste(readLines(result$report, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (text in c("Define Study Area", "What is known", "What to decide next", "A &lt;draft&gt;", "&lt;script&gt;",
    "None recorded; this does not imply none are planned")) expect_match(html, text, fixed = TRUE)
  expect_false(grepl("<script>alert('test')</script>", html, fixed = TRUE))
  expect_false(grepl("MISSING_STUDY_AREA|TERRAIN_REVIEW|RECONSTRUCTION_REQUIRED", html))
  before <- tools::md5sum(result$context)
  define_study_area_report(read_study_context_summary(result$context), file.path(root, "again.html"))
  expect_identical(tools::md5sum(result$context), before)
  expect_error(define_study_area_report(read_study_context_summary(result$context), result$report), "already exists")
})
