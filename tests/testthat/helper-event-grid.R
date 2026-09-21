preflight_fixture <- function(root,reach_polygon=TRUE,no_reaches=FALSE) {
  ids <- .fg_generate_uuid(3)
  polygon <- function(b) sf::st_as_sfc(sf::st_bbox(stats::setNames(b,c("xmin","ymin","xmax","ymax")),crs=26915))
  area <- sf::st_sf(study_area_id=ids[1],study_area_name="Test",geometry=polygon(c(500000.2,4500000.2,500010.8,4500010.8)))
  stream <- sf::st_sf(stream_id=ids[2],study_area_id=ids[1],stream_name="Test Stream",geometry=polygon(c(500002.2,4500002.2,500006.8,4500006.8)))
  reach <- sf::st_sf(reach_id=ids[3],stream_id=ids[2],reach_name="Test Reach",geometry=polygon(c(500003.2,4500003.2,500004.8,4500004.8)))
  if(!reach_polygon) reach <- sf::st_drop_geometry(reach)
  if(no_reaches) reach <- NULL
  crs <- validate_study_analysis_crs(26915)
  context <- file.path(root,"study.gpkg")
  write_study_context(context,area,stream,reach,analysis_reference=data.frame(component="horizontal",value=crs$wkt,
    basis="PROJECT_RECORD",evidence="Test",analyst="Test",recorded_at="2026-09-20T00:00:00Z"))
  prefix <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/Test/Survey/"
  raw <- sf::st_sf(workunit_id=1L,workunit="Synthetic",collect_start=1580515200000,collect_end=1582934400000,
    sourcedem_link=sub("amazonaws.com/","amazonaws.com/index.html?prefix=",prefix,fixed=TRUE),geometry=sf::st_geometry(area))
  d <- list(study_area=area,records=.fg_survey_normalize(raw,"USGS 3DEP","s","now","https://example.org"),
    searches=data.frame(catalog="USGS 3DEP",snapshot_id="s",outcome="COMPLETE"),
    acquisition_plan=data.frame(candidate_key="USGS 3DEP:1",product="DEM"))
  selection <- file.path(root,"selection.gpkg"); write_survey_collection_selection(d,"USGS 3DEP:1",selection)
  group <- file.path(root,"group.gpkg")
  write_survey_acquisition_group(context,selection,"USGS 3DEP:1",ids[2],2020,2,1,"Reviewed acquisition",group)
  raster <- terra::rast(nrows=12,ncols=12,xmin=500000,xmax=500012,ymin=4500000,ymax=4500012,crs="EPSG:26915")
  terra::values(raster) <- rep(c(0,-1.123456789,NA),48)
  tile <- file.path(root,"original.tif"); terra::writeRaster(raster,tile,datatype="FLT8S")
  inventory <- list(stream=stream,collection=d$records,outcome="COMPLETE",message="Test",retrieved_at="now",endpoint="Test",
    files=sf::st_sf(file_id="tile1",title="Test tile",download_url=paste0(prefix,"tile.tif"),size_bytes=file.info(tile)$size,
      format="GeoTIFF",raw_metadata="{}",geometry=sf::st_transform(sf::st_geometry(area),4326)))
  files <- file.path(root,"files.gpkg");write_stream_dem_selection(inventory,"tile1",files,basename(context))
  attempt <- prepare_stream_dem_download(files,file.path(root,"source-dem"))
  with_mocked_bindings(run_stream_dem_download(attempt),.fg_dem_transfer=function(url,path,...) {
    file.copy(tile,path)
    list(status_code=200L,headers=charToRaw(paste0("HTTP/1.1 200 OK\r\nContent-Length: ",file.info(tile)$size,"\r\n\r\n")))
  })
  list(context=context,selection=selection,group=group,stream_id=ids[2],
    sources=data.frame(candidate_key="USGS 3DEP:1",selection_path=files,attempt=attempt))
}
