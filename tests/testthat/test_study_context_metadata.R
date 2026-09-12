metadata_fixture <- function(root) {
  aid <- .fg_generate_uuid(1); sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1); eid <- .fg_generate_uuid(2)
  ctx <- write_study_context(file.path(root,"study.gpkg"),study_area=data.frame(study_area_id=aid,study_area_name="Synthetic metadata test"),
    streams=data.frame(stream_id=sid,study_area_id=aid,stream_name="Stream"),reaches=data.frame(reach_id=rid,stream_id=sid,reach_name="R1"),
    survey_events=data.frame(survey_event_id=eid,reach_id=rid,survey_year=c(2006L,2010L)))
  dem <- terra::rast(nrows=2,ncols=2,crs="EPSG:26914",xmin=0,xmax=2,ymin=0,ymax=2)
  terra::values(dem) <- c(NA,0,-1,5); tif <- file.path(root,"test.tif"); terra::writeRaster(dem,tif)
  a <- associate_study_terrain(ctx,file.path(root,"a.gpkg"),eid[1],tif,"Test","Fixture",file.path(root,"a.json"))
  b <- associate_study_terrain(a$context,file.path(root,"b.gpkg"),eid[2],tif,"Shared test","Fixture",file.path(root,"b.json"))
  list(context=b$context,manifest=b$manifest,event=eid[1],tif=tif)
}

test_that("partial evidenced metadata preserves masks, IDs, snapshots and shared links", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- metadata_fixture(root); original <- jsonlite::read_json(f$manifest)
  hashes <- tools::md5sum(list.files(root,full.names=TRUE))
  a <- record_study_terrain_metadata(f$context,file.path(root,"units.gpkg"),f$event,"m",NULL,
    "Synthetic known units, not Cole Creek evidence","Fixture",file.path(root,"units.json"))
  x <- jsonlite::read_json(a$manifest)
  expect_identical(x$event_links,original$event_links)
  expect_identical(x$artifacts[[1]]$sha256,original$artifacts[[1]]$sha256)
  expect_identical(x$artifacts[[1]]$observed,original$artifacts[[1]]$observed)
  expect_null(x$artifacts[[1]]$vertical_reference)
  expect_true("VERTICAL_REFERENCE_UNKNOWN" %in% inspect_terrain_folder(a$manifest)$assessment$code)
  b <- record_study_terrain_metadata(a$context,file.path(root,"reference.gpkg"),f$event,"","Test datum",
    "Synthetic reference evidence","Fixture",file.path(root,"reference.json"),file.path(root,"report.html"))
  y <- jsonlite::read_json(b$manifest)
  expect_identical(y$artifacts[[1]]$vertical_unit,"m")
  expect_match(y$artifacts[[1]]$metadata_evidence,"Synthetic known units",fixed=TRUE)
  expect_match(y$artifacts[[1]]$metadata_evidence,"Synthetic reference evidence",fixed=TRUE)
  expect_false("VERTICAL_REFERENCE_UNKNOWN" %in% inspect_terrain_folder(b$manifest)$assessment$code)
  saved <- read_study_context(b$context); old <- read_study_context(f$context)
  saved$folder_manifest <- old$folder_manifest <- NULL; expect_identical(saved,old)
  expect_identical(tools::md5sum(names(hashes)),hashes)
  html <- xml2::xml_text(xml2::read_html(b$report))
  expect_match(html,"Synthetic reference evidence",fixed=TRUE)
  expect_false(grepl("Terrain coverage review|Reach with finite data|Raster cells with finite data",html))
  expect_match(html,"NoData masking",fixed=TRUE)
  expect_error(record_study_terrain_metadata(b$context,file.path(root,"bad.gpkg"),f$event,"ft",NULL,"Test","Fixture",file.path(root,"bad.json")),"cannot be replaced")
  expect_false(file.exists(file.path(root,"bad.json")))
})

test_that("metadata entry refuses unsafe input without publishing outputs", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- metadata_fixture(root)
  p <- list(dsn=f$context,output_file=file.path(root,"out.gpkg"),survey_event_id=f$event,
    vertical_unit="m",evidence="Synthetic",analyst="Fixture",manifest_file=file.path(root,"out.json"))
  for (override in list(list(survey_event_id="unknown"),list(vertical_unit=""),list(evidence=" "),
      list(analyst=" "),list(output_file=f$context),list(manifest_file=f$manifest))) {
    expect_error(do.call(record_study_terrain_metadata,utils::modifyList(p,override)))
    expect_false(file.exists(p$output_file)); expect_false(file.exists(p$manifest_file))
  }
  a <- do.call(record_study_terrain_metadata,p)
  expect_error(record_study_terrain_metadata(a$context,file.path(root,"same.gpkg"),f$event,"m",NULL,"Again","Fixture",file.path(root,"same.json")),"No new metadata")
  dem <- terra::rast(f$tif)+1; terra::writeRaster(dem,f$tif,overwrite=TRUE)
  expect_error(record_study_terrain_metadata(a$context,file.path(root,"bad.gpkg"),f$event,NULL,"Test datum","Test","Fixture",file.path(root,"bad.json")),"integrity or metadata conflicts")
  expect_false(file.exists(file.path(root,"bad.json")))
})
