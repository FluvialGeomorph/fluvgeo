# Run from fluvgeo. Focused regression checks and isolated help regeneration.
pkgload::load_all("../fluvgeodata", quiet = TRUE)
testthat::test_local(".", filter = "legacy_staging|study_context|terrain_(development_report|manifest|event_links)",
  reporter = "summary")
docs <- tempfile("legacy-staging-help-"); dir.create(docs)
dir.create(file.path(docs, "R"))
file.copy("DESCRIPTION", docs)
file.copy(c("R/legacy_staging.R", "R/terrain_development_report.R"), file.path(docs, "R"))
roxygen2::roxygenise(docs, roclets = "rd", load_code = roxygen2::load_source)
for (topic in c("inspect_legacy_staging.Rd", "terrain_development_summary.Rd", "terrain_development_report.Rd", "study_staging_report.Rd")) {
  stopifnot(file.copy(file.path(docs, "man", topic), file.path("man", topic), overwrite = TRUE))
}
