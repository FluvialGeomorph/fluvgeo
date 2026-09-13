# From workspace root: Rscript --vanilla <this script> <new output directory>.
# Synthetic new-study configuration only, not choices assigned to Cole Creek.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]))
dir.create(args[1],recursive=TRUE)
root <- normalizePath(args[1],winslash="/",mustWork=TRUE)
pkgload::load_all("fluvgeo",quiet=TRUE)
start <- fluvgeo::start_study_context(file.path(root,"draft.gpkg"),
  "DEVELOPMENT EXAMPLE - synthetic analysis-reference choices",
  "Demonstrates saving proposed choices only. These are not decisions about Cole Creek or any customer project. No terrain or acquired survey is supplied; the vertical reference intentionally remains unresolved.")
one <- fluvgeo::record_study_analysis_reference(start$context,file.path(root,"horizontal.gpkg"),
  "horizontal","EPSG:26914 - demonstration candidate","PROPOSED",
  "Synthetic example only; an analyst would evaluate customer scope, source data and interoperability before choosing.",
  "Development example")
two <- fluvgeo::record_study_analysis_reference(one$context,file.path(root,"analysis-choices.gpkg"),
  "elevation_unit","International foot (0.3048 m)","PROPOSED",
  "Synthetic example, not an assertion about retained terrain. Recording a choice does not convert elevations or establish a vertical datum.",
  "Development example",report_file=file.path(root,"define-study-area.html"))
stopifnot(is.null(fluvgeo::read_study_context_summary(two$context)$terrain_review))
cat(two$report,"\n")
