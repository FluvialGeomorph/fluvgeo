# Run from workspace root with one NEW output directory. All evidence is synthetic.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]))
dir.create(args[1],recursive=TRUE)
root <- normalizePath(args[1],winslash="/",mustWork=TRUE)
pkgload::load_all("fluvgeo",quiet=TRUE)
dem <- terra::rast(nrows=3,ncols=3,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:26914")
terra::values(dem) <- c(NA,1:7,NA)
terra::writeRaster(dem,file.path(root,"synthetic-terrain.tif"))
fluvgeo::write_terrain_manifest(root,data.frame(artifact_id="terrain",
  path="synthetic-terrain.tif",role="Synthetic demonstration"),"Preparation account demonstration")
start <- fluvgeo::start_study_context(file.path(root,"draft.gpkg"),
  "DEVELOPMENT EXAMPLE - recorded terrain preparation",
  "All terrain, source identities and preparation accounts are synthetic. No Cole Creek or customer lineage is asserted. The described operations were not run.")$context
context <- fluvgeo::read_study_context(start); context$folder_manifest <- "terrain-manifest.json"
initial <- do.call(fluvgeo::write_study_context,c(list(dsn=file.path(root,"inventory.gpkg")),context))
claim <- fluvgeo::record_study_terrain_source(initial,file.path(root,"claim.gpkg"),
  "example-source","terrain","DEMO_ARCHIVE","record-A","Synthetic archive inventory",
  "Example collection; actual use unresolved","CANDIDATE","PROJECT_RECORD",
  "Possible match only. No collection is established as the source of this terrain.","Development example")$context
note <- file.path(root,"synthetic-processing-note.txt")
writeLines(c("SYNTHETIC DOCUMENT - NOT AN EXECUTION LOG",
  "Example account: first derive a DEM, then calculate DEM / 0.3048.",
  "Exact inputs, interpolation, software version and execution dates are unknown."),note)
retained <- fluvgeo::retain_study_terrain_evidence(claim,file.path(root,"retained.gpkg"),
  "example-note","example-source",note,"PROCESSING_RECORD","Synthetic preparation note",
  "Demonstration only","This document is not evidence of actual execution.","Development example")$context
steps <- data.frame(operation=c("Derive DEM", "Convert elevation values"),
  input_description=c("Example collection; exact point-cloud files unknown", "Derived DEM described in step 1"),
  output_description=c("Derived DEM; intermediate not retained", "Analysis terrain described by this account"),
  parameters=c(NA_character_,"DEM / 0.3048; supplied literal expression, not executed"),
  software=c(NA_character_,"ArcMap (supplied example account)"),
  software_version=rep(NA_character_,2),execution_time=rep(NA_character_,2))
files <- list.files(root,recursive=TRUE,full.names=TRUE); hashes <- tools::md5sum(files)
dir.create(file.path(root,"report"))
out <- fluvgeo::record_study_terrain_processing(retained,file.path(root,"processing.gpkg"),
  "example-preparation","example-source",steps,"PROJECT_RECORD",
  "Synthetic interpretation of the retained note. This does not establish source use, actual execution, a vertical datum transformation, or exact input/output bytes.",
  "Development example",evidence_id="example-note",report_file=file.path(root,"report/define-study-area.html"))
summary <- fluvgeo::read_study_context_summary(out$context)
stopifnot(identical(tools::md5sum(files),hashes),
  identical(summary$terrain_sources$status,"CANDIDATE"),
  identical(summary$terrain_processing$step_number,1:2),
  identical(summary$retained_evidence$integrity,"MATCH"))
cat(out$report,"\n")
