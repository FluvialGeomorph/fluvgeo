# From workspace root: Rscript --vanilla <this script> <NEW output directory>.
# Completely synthetic source claims and terrain; no Cole Creek attribution.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]))
dir.create(args[1],recursive=TRUE)
root <- normalizePath(args[1],winslash="/",mustWork=TRUE)
pkgload::load_all("fluvgeo",quiet=TRUE)
dem <- terra::rast(nrows=3,ncols=3,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:26914")
terra::values(dem) <- c(NA,1:7,NA)
terra::writeRaster(dem,file.path(root,"synthetic-terrain.tif"))
fluvgeo::write_terrain_manifest(root,data.frame(artifact_id="demo-terrain",
  path="synthetic-terrain.tif",role="Synthetic example; not an observed survey"),"Source-use demonstration")
start <- fluvgeo::start_study_context(file.path(root,"draft.gpkg"),
  "DEVELOPMENT EXAMPLE - synthetic terrain source-use evidence",
  "Demonstrates evidence recording only. Neither the terrain nor its source claims describe Cole Creek or a customer project. No survey acquisition or transformation is asserted.")
context <- fluvgeo::read_study_context(start$context)
context$folder_manifest <- "terrain-manifest.json"
initial <- do.call(fluvgeo::write_study_context,c(list(dsn=file.path(root,"inventory.gpkg")),context))
files <- list.files(root,full.names=TRUE); hashes <- tools::md5sum(files)
one <- fluvgeo::record_study_terrain_source(initial,file.path(root,"candidate.gpkg"),
  "possible-source","demo-terrain","DEMO_CATALOG","record-A","Synthetic catalog snapshot A",
  "Example candidate collection","CANDIDATE","PROJECT_RECORD",
  "Illustrative spatial/date lead only; no evidence of actual use has been recovered.","Development example")
two <- fluvgeo::record_study_terrain_source(one$context,file.path(root,"recorded-use.gpkg"),
  "recalled-source","demo-terrain","DEMO_ARCHIVE","record-B","Synthetic archive inventory B",
  "Example recalled source","RECORDED_USE","OWNER_RECOLLECTION",
  "Synthetic attributed account of historical use; exact source edition, inputs and processing steps remain unknown.","Development example")
three <- fluvgeo::record_study_terrain_source(two$context,file.path(root,"review.gpkg"),
  "ruled-out-source","demo-terrain","DEMO_ARCHIVE","record-C","Synthetic project notes C",
  "Example ruled-out collection","REJECTED","PROJECT_RECORD",
  "Illustrative project notes rule out this source. Keep the rationale to avoid repeating the same investigation.",
  "Development example",report_file=file.path(root,"define-study-area.html"))
stopifnot(identical(tools::md5sum(files),hashes))
cat(three$report,"\n")
