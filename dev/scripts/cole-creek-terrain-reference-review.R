# Workspace-root demo. Builds a NEW review RDS, never edits terrain or manifests.
# Usage: Rscript .../cole-creek-terrain-reference-review.R <new-review.rds>
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,!file.exists(args[1]),dir.exists(dirname(args[1])))
pkgload::load_all('fluvgeo',quiet=TRUE)
root <- 'fluvgeo/dev/outputs/terrain-development/cole-creek-folder-v3'
manifest <- jsonlite::read_json(file.path(root,'terrain-manifest.json'))
selected <- Filter(function(x) x$artifact_id %in% paste0('cole-',c(2006,2010,2016),'-raster-1'),manifest$artifacts)
stopifnot(length(selected)==3L)
artifacts <- data.frame(artifact_id=vapply(selected,`[[`,character(1),'artifact_id'),
  label=vapply(selected,`[[`,character(1),'role'),role='ANALYSIS_DEM',
  path=file.path(root,vapply(selected,`[[`,character(1),'path')),
  evidence='Existing Cole Creek folder-v3 intake and geotiff-copy-evidence.json identify these retained analysis-raster copies; no new conversion.',
  review_note=NA_character_,review_evidence=NA_character_)
for (i in seq_len(nrow(artifacts))) {
  con <- file(artifacts$path[i],'rb')
  hash <- unclass(as.character(openssl::sha256(con))); close(con)
  stopifnot(identical(tolower(hash),tolower(selected[[i]]$sha256)))
}
source_file <- 'fluvgeo/dev/outputs/terrain-development/source-header-v1/douglas-2022-135000_113500.tif'
con <- file(source_file,'rb'); hash <- unclass(as.character(openssl::sha256(con))); close(con)
stopifnot(tolower(hash)=='9eddbf267f4bc99f7eb75e1290bbd66ae694a04d06a0c02015e96c1d04b63b3f')
sources <- data.frame(artifact_id=c('omaha-2013-candidate','douglas-2022-sample'),
  label=c('2013 Omaha candidate (no complete local GeoTIFF selected)',
          '2022 Douglas County candidate: tile 135000_113500'),
  role='SOURCE_PRODUCT',path=c(NA_character_,source_file),
  evidence=c('USIEI 39542; USGS Omaha source-access review. IMG header alone is not a complete terrain payload.',
    'USIEI 47476; public Douglas County sample and retained source-header SHA-256; candidate, not a linked FG source.'),
  review_note=c('The project report records flights April 19-25, 2013; the June catalog label is retained raw. Resolve conflicting source CRS statements and reuse/distribution terms before selecting a complete analysis subset.',
    'Resolve source-use agreement and differing acquisition-date scopes. This sample intersects only part of Cole Creek R1.'),
  review_evidence='fluvgeo/dev/features/cole-creek-source-access-review.md; public metadata/header review on 2026-09-12')
analysis <- data.frame(component='elevation_unit',
  value='Feet: historic FG convention; not independently verified for these files',
  basis='OWNER_RECOLLECTION',
  evidence=paste('Project owner, 2026-09-12: typical meter DEM conversion used 0.3048;',
    'division implies international feet, but exact per-project recipes are not retained here.',
    'Source and chosen analysis references may differ. See FGDB ADR-0026 and legacy-esri-elevation-feet.md.'))
review <- terrain_reference_review(rbind(sources,artifacts),analysis)
saveRDS(review,args[1])
print(review$files[c('label','inspection_status','vertical','vertical_unit')])
