# Workspace root; one NEW output directory. All terrain and claims are synthetic.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]))
dir.create(args[1],recursive=TRUE)
root <- normalizePath(args[1],winslash="/",mustWork=TRUE)
pkgload::load_all("fluvgeo",quiet=TRUE)
dem <- terra::rast(nrows=3,ncols=3,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:26914")
terra::values(dem) <- c(NA,1:7,NA)
terra::writeRaster(dem,file.path(root,"synthetic-terrain.tif"))
fluvgeo::write_terrain_manifest(root,data.frame(artifact_id="terrain",
  path="synthetic-terrain.tif",role="Synthetic demonstration"),"Evidence retention demonstration")
start <- fluvgeo::start_study_context(file.path(root,"draft.gpkg"),
  "DEVELOPMENT EXAMPLE - retained terrain evidence",
  "All terrain, source identities and supporting records are synthetic. No Cole Creek or customer provenance is asserted.")$context
context <- fluvgeo::read_study_context(start); context$folder_manifest <- "terrain-manifest.json"
initial <- do.call(fluvgeo::write_study_context,c(list(dsn=file.path(root,"inventory.gpkg")),context))
claim <- fluvgeo::record_study_terrain_source(initial,file.path(root,"claim.gpkg"),
  "example-source","terrain","DEMO_ARCHIVE","record-A","Synthetic inventory record A",
  "Example collection; actual use unresolved","CANDIDATE","PROJECT_RECORD",
  "A possible match requiring investigation; the demonstration documents establish no actual source use.","Development example")$context
dir.create(file.path(root,"selected-originals"))
metadata <- file.path(root,"selected-originals/metadata.json")
log <- file.path(root,"selected-originals/processing-note.txt")
jsonlite::write_json(list(synthetic=TRUE,record="record-A",source_edition=NULL,
  note="Demonstration metadata only; no observed collection."),metadata,pretty=TRUE,auto_unbox=TRUE)
writeLines(c("SYNTHETIC DOCUMENT — NOT AN EXECUTION LOG",
  "An example note mentions a meters-to-feet raster calculation.",
  "No input files, software version, execution or output relationship are established."),log,useBytes=TRUE)
files <- list.files(root,recursive=TRUE,full.names=TRUE); hashes <- tools::md5sum(files)
one <- fluvgeo::retain_study_terrain_evidence(claim,file.path(root,"metadata.gpkg"),
  "metadata-record","example-source",metadata,"METADATA_SNAPSHOT",
  "Example consulted metadata","Synthetic inventory record A; edition unknown",
  "Preserved bytes are not proof that this collection was used.","Development example")$context
two <- fluvgeo::retain_study_terrain_evidence(one,file.path(root,"review.gpkg"),
  "processing-record","example-source",log,"PROCESSING_RECORD",
  "Example archived processing note","Synthetic project note; execution date unknown",
  "This supplied note does not establish actual execution, exact inputs or transformation correctness.",
  "Development example",report_file=file.path(root,"define-study-area.html"))
stopifnot(identical(tools::md5sum(files),hashes),
  all(fluvgeo::read_study_context_summary(two$context)$retained_evidence$integrity=="MATCH"))
cat(two$report,"\n")
