# Run from fluvgeo. Focused backend checks and isolated help generation only.
pkgload::load_all("../fluvgeodata", quiet = TRUE)
testthat::test_local(".", filter = "study_context|terrain_(development_report|manifest|event_links)", reporter = "summary")
docs <- tempfile("study-context-help-"); dir.create(docs)
dir.create(file.path(docs, "R"))
file.copy("DESCRIPTION", docs)
file.copy(c("R/study_context.R", "R/revise_study_context.R"), file.path(docs, "R"))
roxygen2::roxygenise(docs, roclets = "rd", load_code = roxygen2::load_source)
for (topic in c("write_study_context.Rd", "read_study_context.Rd", "revise_study_context.Rd")) {
  stopifnot(file.copy(file.path(docs, "man", topic), file.path("man", topic), overwrite = TRUE))
}
