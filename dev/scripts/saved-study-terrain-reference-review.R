# Run from the workspace root: <saved-context.gpkg> <new-report.html>.
# Developer reproduction only. Inputs are existing local files, never rewritten.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L,file.exists(args[1]),!file.exists(args[2]),dir.exists(dirname(args[2])))
pkgload::load_all('fluvgeo',quiet=TRUE)
context <- fluvgeo::read_study_context(args[1])
paths <- args[1]
if (!is.null(context$folder_manifest)) {
  manifest <- jsonlite::read_json(context$folder_manifest)
  paths <- c(paths,context$folder_manifest,file.path(dirname(context$folder_manifest),
    vapply(manifest$artifacts,`[[`,character(1),'path')))
}
paths <- paths[file.exists(paths)]
before <- tools::md5sum(paths)
# Do not infer a project-wide choice from source metadata or the FG feet convention.
summary <- fluvgeo::read_study_context_summary(args[1],terrain_references=TRUE)
fluvgeo::define_study_area_report(summary,args[2])
stopifnot(identical(before,tools::md5sum(paths)))
cat('Rendered saved Study Area with',nrow(summary$terrain_review$files),
  'explicitly selected DEMs; inputs unchanged.\n')
print(summary$terrain_review$files[c('artifact_id','inspection_status')])
