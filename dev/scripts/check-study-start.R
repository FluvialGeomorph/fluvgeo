# Focused offline checks; run from fluvgeo with the workstation's Pandoc configured.
pkgload::load_all("../fluvgeodata", quiet = TRUE)
testthat::test_local(".", filter = "start_study_context|study_context|terrain_(development_report|manifest|event_links)", reporter = "summary", stop_on_failure = TRUE)
docs <- tempfile("study-start-help-"); dir.create(docs)
dir.create(file.path(docs, "R"))
file.copy("DESCRIPTION", docs)
file.copy("R/start_study_context.R", file.path(docs, "R"))
roxygen2::roxygenise(docs, roclets = c("rd", "namespace"), load_code = roxygen2::load_source)
for (topic in c("start_study_context.Rd", "define_study_area_report.Rd")) {
  stopifnot(file.copy(file.path(docs, "man", topic), file.path("man", topic), overwrite = TRUE))
}
