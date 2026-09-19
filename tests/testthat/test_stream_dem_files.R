dem_file_fixture <- function() {
  stream <- sf::st_sf(stream_id="stream-test",geometry=sf::st_as_sfc(sf::st_bbox(
    c(xmin=-96.10,ymin=41.25,xmax=-96.09,ymax=41.26),crs=4326)))
  collection <- sf::st_sf(candidate_key="USGS 3DEP:test",catalog="USGS 3DEP",snapshot_id="snapshot",status="Published",
    access_url="Unknown",raw_metadata='{"sourcedem_link":"https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Elevation/OPR/Projects/Project/Survey","dem_gsd_meters":0.5}',
    geometry=sf::st_geometry(stream))
  item <- list(sourceId="tile-one",title="Synthetic tile",downloadURL="https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/Project/Survey/TIFF/tile.tif",
    sizeInBytes=1000,format="GeoTIFF",boundingBox=list(minX=-96.11,minY=41.24,maxX=-96.08,maxY=41.27))
  list(stream=stream,collection=collection,item=item)
}

test_that("file query matches source directory and intersects Stream with sf", {
  f <- dem_file_fixture(); other <- f$item; other$downloadURL <- sub("Survey/","SurveyOther/",other$downloadURL)
  outside <- f$item; outside$sourceId <- "outside"; outside$downloadURL <- sub("tile.tif","outside.tif",outside$downloadURL)
  outside$boundingBox <- list(minX=-95,minY=40,maxX=-94,maxY=41)
  with_mocked_bindings({
    r <- discover_stream_dem_files(f$stream,f$collection)
    expect_identical(r$outcome,"COMPLETE"); expect_equal(nrow(r$files),1L)
    expect_identical(r$files$file_id,"tile-one"); expect_equal(r$files$pixel_size_m,.5)
    expect_match(r$files$resolution_evidence,"verify raster")
  },.fg_dem_products_get=function(query) {
    expect_match(query$datasets,"Original Product")
    expect_identical(query$bbox,paste(as.numeric(sf::st_bbox(f$stream)),collapse=","))
    list(total=3,items=list(f$item,other,outside),errors=list())
  })
})

test_that("file query distinguishes partial, unsupported, empty and failure", {
  f <- dem_file_fixture()
  with_mocked_bindings({
    expect_identical(discover_stream_dem_files(f$stream,f$collection,1)$outcome,"PARTIAL")
  },.fg_dem_products_get=function(...) list(total=2,items=list(f$item),errors=list()))
  with_mocked_bindings({
    expect_identical(discover_stream_dem_files(f$stream,f$collection)$outcome,"FAILED")
  },.fg_dem_products_get=function(...) stop("Synthetic timeout"))
  with_mocked_bindings({
    r <- discover_stream_dem_files(f$stream,f$collection)
    expect_identical(r$outcome,"COMPLETE");expect_equal(nrow(r$files),0L)
    expect_match(r$message,"selected Stream",fixed=TRUE)
    expect_match(r$message,"other parts of the Study Area",fixed=TRUE)
  },.fg_dem_products_get=function(...) list(total=0,items=list(),errors=list()))
  f$collection$catalog <- "USIEI"
  expect_identical(discover_stream_dem_files(f$stream,f$collection)$outcome,"UNSUPPORTED")
  expect_null(.fg_dem_source_prefix("https://example.org/?prefix=anything"))
  expect_null(.fg_dem_source_prefix("https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Elevation/OPR/Projects/Project/../Survey"))
  expect_error(discover_stream_dem_files(sf::st_drop_geometry(f$stream),f$collection),"Stream polygon")
})

test_that("malformed matched file evidence fails closed", {
  f <- dem_file_fixture(); f$item$boundingBox <- NULL
  with_mocked_bindings({
    r <- discover_stream_dem_files(f$stream,f$collection)
    expect_identical(r$outcome,"FAILED");expect_equal(nrow(r$files),0L)
  },.fg_dem_products_get=function(...) list(total=1,items=list(f$item),errors=list()))
})

test_that("file choice snapshots are immutable and retain evidence", {
  f <- dem_file_fixture()
  with_mocked_bindings({r <- discover_stream_dem_files(f$stream,f$collection)},
    .fg_dem_products_get=function(...) list(total=1,items=list(f$item),errors=list()))
  path <- tempfile(fileext=".gpkg");on.exit(unlink(path))
  expect_error(write_stream_dem_selection(r,"forged",path,"revision-1"),"reviewed inventory")
  write_stream_dem_selection(r,"tile-one",path,"revision-1")
  v <- read_stream_dem_selection(path)
  expect_identical(v$selected,"tile-one");expect_identical(v$context_revision,"revision-1")
  expect_identical(v$files$raw_metadata,r$files$raw_metadata)
  expect_error(write_stream_dem_selection(r,"tile-one",path,"revision-1"),"already exists")
  r$outcome <- "FAILED"
  expect_error(write_stream_dem_selection(r,character(),tempfile(fileext=".gpkg"),"revision-1"),"reviewed inventory")
})
