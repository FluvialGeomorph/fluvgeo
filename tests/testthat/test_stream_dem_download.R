download_fixture <- function(n=1L) {
  dir <- tempfile("dem-download-"); dir.create(dir)
  s <- sf::st_sf(stream_id="synthetic-stream",geometry=sf::st_as_sfc(sf::st_bbox(
    c(xmin=-96.10,ymin=41.25,xmax=-96.09,ymax=41.26),crs=4326)))
  prefix <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/Test/Survey/"
  c <- sf::st_sf(candidate_key="USGS 3DEP:synthetic",catalog="USGS 3DEP",snapshot_id="snapshot",
    status="Published",access_url="Unknown",raw_metadata=paste0('{"sourcedem_link":"',
      sub("amazonaws.com/","amazonaws.com/index.html?prefix=",prefix,fixed=TRUE),'","dem_gsd_meters":1}'),
    geometry=sf::st_geometry(s))
  body <- c(as.raw(c(73,73,42,0,8,0,0,0)),as.raw(rep(0,24)))
  r <- list(stream=s,collection=c,outcome="COMPLETE",message="Synthetic only",retrieved_at="now",endpoint="synthetic",
    files=sf::st_sf(file_id=paste0("tile",seq_len(n)),title=paste("Synthetic",seq_len(n)),
      download_url=paste0(prefix,"tile",seq_len(n),".tif"),size_bytes=length(body),format="GeoTIFF",raw_metadata="{}",
      geometry=rep(sf::st_geometry(s),n)))
  selection <- file.path(dir,"selected.gpkg")
  write_stream_dem_selection(r,r$files$file_id,selection,"revision-000001.gpkg")
  list(dir=dir,selection=selection,destination=file.path(dir,"source-dem"),inventory=r,body=body)
}
download_response <- function(n=32,status=200L,headers=NULL) {
  if(is.null(headers)) headers <- paste0("Content-Length: ",n,"\r\nContent-Type: image/tiff\r\n")
  list(status_code=status,headers=charToRaw(paste0("HTTP/1.1 ",status," Test\r\n",headers,"\r\n")))
}

test_that("display scaling preserves source NoData and row order", {
  root <- tempfile("scaled-preview-");dir.create(root);on.exit(unlink(root,recursive=TRUE))
  raw <- file.path(root,"raw.tif");source <- file.path(root,"scaled.tif")
  r <- terra::rast(nrows=2,ncols=3,xmin=0,xmax=3,ymin=0,ymax=2,crs="EPSG:26914")
  terra::values(r) <- c(0,-1,2,NA,4,5);terra::writeRaster(r,raw,datatype="INT2S",NAflag=-9999)
  # One valid scaled value equals the original raw NoData sentinel.
  sf::gdal_utils("translate",raw,source,options=c("-a_scale","2","-a_offset","-10003"),quiet=TRUE)
  observation <- inspect_terrain_vertical_reference(source)
  before <- tools::md5sum(list.files(root,full.names=TRUE))
  with_mocked_bindings({
    p <- preview_stream_dem_download("synthetic","tile")
    expect_equal(p$preview$values,matrix(c(-10003,-10005,-9999,NA,-9995,-9993),2,3,byrow=TRUE))
    expect_identical(tools::md5sum(list.files(root,full.names=TRUE)),before)
  },inspect_stream_dem_download=function(...) list(observation=observation,sha256=observation$sha256))
})

test_that("inspection binds real GeoTIFF metadata to its receipt without writes", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  source <- file.path(f$dir,"grid.tif")
  raster <- terra::rast(nrows=2,ncols=3,xmin=500000,xmax=500003,
    ymin=4500000,ymax=4500002,crs="EPSG:26914")
  terra::values(raster) <- c(0,-1,2,NA,4,5)
  terra::writeRaster(raster,source,datatype="FLT4S")
  f$inventory$files$size_bytes <- file.info(source)$size
  selection <- file.path(f$dir,"real.gpkg")
  write_stream_dem_selection(f$inventory,"tile1",selection,"revision-000001.gpkg")
  with_mocked_bindings({
    a <- prepare_stream_dem_download(selection,f$destination)
    expect_error(inspect_stream_dem_download(a,"tile1"),"no valid local")
    d <- run_stream_dem_download(a)
    before <- tools::md5sum(list.files(f$destination,recursive=TRUE,full.names=TRUE))
    x <- inspect_stream_dem_download(a,"tile1")
    expect_equal(x$observation$internal_compound$grid$size,c(3,2))
    expect_equal(x$observation$internal_compound$grid$spacing,c(1,1))
    expect_match(x$observation$internal_compound$grid$horizontal_unit,"met")
    expect_identical(x$observation$internal_compound$status,"VERTICAL_CRS_NOT_EXPOSED")
    p <- preview_stream_dem_download(a,"tile1")
    expect_equal(p$preview$values,matrix(c(0,-1,2,NA,4,5),2,3,byrow=TRUE))
    expect_equal(p$preview$sampled_size,c(3,2))
    expect_true(p$preview$native)
    expect_equal(p$preview$window,c(0,0,3,2))
    detail <- preview_stream_dem_download(a,"tile1",window=c(1,0,2,2))
    expect_equal(detail$preview$values,matrix(c(-1,2,4,5),2,2,byrow=TRUE))
    expect_true(detail$preview$native)
    expect_equal(preview_stream_dem_download(a,"tile1",window=c(0,1,1,1))$preview$values,matrix(NA_real_,1,1))
    expect_error(preview_stream_dem_download(a,"tile1",window=c(2,0,2,2)),"inside the source")
    expect_error(preview_stream_dem_download(a,"tile1",window=c(0,0,1.5,1)),"integer")
    expect_error(preview_stream_dem_download(a,"tile1",window=c(-1,0,1,1)),"integer")
    expect_false(preview_stream_dem_download(a,"tile1",2)$preview$native)
    expect_equal(preview_stream_dem_download(a,"tile1",2)$preview$sampled_size,c(2,1))
    expect_error(preview_stream_dem_download(a,"tile1",513),"max_dimension")
    expect_identical(tools::md5sum(list.files(f$destination,recursive=TRUE,full.names=TRUE)),before)
    expect_error(inspect_stream_dem_download(a,"missing"),"no valid local")
    cache <- file.path(f$dir,"view-cache")
    cached <- preview_stream_dem_download(a,"tile1",cache_dir=cache)
    with_mocked_bindings({
      expect_equal(preview_stream_dem_download(a,"tile1",cache_dir=cache)$preview,cached$preview)
      expect_equal(preview_stream_dem_download(a,"tile1",window=c(1,0,2,2),cache_dir=cache)$preview$values,
        matrix(c(-1,2,4,5),2,2,byrow=TRUE))
      expect_error(inspect_stream_dem_download(a,"tile1",cache_dir=cache,refresh=TRUE),"integrity read")
    },.fg_file_sha256=function(...) stop("integrity read"))
    asset <- file.path(f$destination,d$files$asset)
    bytes <- readBin(asset,"raw",file.info(asset)$size);bytes[length(bytes)] <- as.raw(127)
    writeBin(bytes,asset)
    expect_error(inspect_stream_dem_download(a,"tile1"),"checksum")
    expect_error(preview_stream_dem_download(a,"tile1"),"checksum")
    expect_error(preview_stream_dem_download(a,"tile1",cache_dir=cache),"checksum")
  },.fg_dem_transfer=function(url,path,...) {
    file.copy(source,path);download_response(file.info(source)$size)
  })
})

test_that("immutable attempts publish, reopen, reuse and preserve corrupt earlier assets", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  count <- 0
  with_mocked_bindings({
    a <- prepare_stream_dem_download(f$selection,f$destination)
    expect_identical(read_stream_dem_download(a)$files$outcome,"NOT_STARTED")
    x <- run_stream_dem_download(a)
    expect_identical(x$files$outcome,"DOWNLOADED")
    expect_identical(read_stream_dem_download(a,FALSE)$files$outcome,"RECORDED")
    expect_error(run_stream_dem_download(a),"publish")
    b <- prepare_stream_dem_download(f$selection,f$destination)
    expect_identical(run_stream_dem_download(b)$files$outcome,"REUSED")
    expect_equal(count,1)
    asset <- file.path(f$destination,x$files$asset)
    writeBin(as.raw(rep(1,32)),asset)
    expect_identical(read_stream_dem_download(a)$files$outcome,"UNAVAILABLE")
    d <- prepare_stream_dem_download(f$selection,f$destination)
    y <- run_stream_dem_download(d)
    expect_identical(y$files$outcome,"DOWNLOADED");expect_equal(count,2)
    expect_false(identical(y$files$asset,x$files$asset))
    expect_true(file.exists(asset))
    expect_identical(readBin(asset,"raw",32),as.raw(rep(1,32)))
    moved <- paste0(f$dir,"-moved");fs::dir_copy(f$dir,moved);on.exit(unlink(moved,recursive=TRUE),add=TRUE)
    expect_identical(read_stream_dem_download(file.path(moved,"source-dem","attempts",basename(d)))$files$outcome,"DOWNLOADED")
  },.fg_dem_transfer=function(url,path,limits,cancelled,progress) {
    count <<- count+1;writeBin(f$body,path);progress(32,32);download_response()
  })
})

test_that("failed transfers, interrupted attempts and explicit retries preserve partial success", {
  f <- download_fixture(2);on.exit(unlink(f$dir,recursive=TRUE))
  with_mocked_bindings({
    a <- prepare_stream_dem_download(f$selection,f$destination)
    x <- run_stream_dem_download(a)
    expect_identical(x$files$outcome,c("DOWNLOADED","FAILED"))
    expect_match(x$files$message[2],"Catalog size")
    expect_length(list.files(file.path(a,"incomplete")),0L)
    b <- prepare_stream_dem_download(f$selection,f$destination)
    .fg_dem_json(list(index=2L),file.path(b,"progress.json"),FALSE)
    writeBin(f$body,file.path(b,"incomplete","000002.part"))
    expect_identical(read_stream_dem_download(b)$files$outcome,c("NOT_STARTED","INTERRUPTED"))
    cancel_stream_dem_download(b)
    expect_identical(read_stream_dem_download(b)$files$outcome,c("NOT_STARTED","CANCELLED"))
    expect_length(list.files(file.path(b,"incomplete")),0L)
    expect_true(file.exists(file.path(f$destination,x$files$asset[1])))
  },.fg_dem_transfer=function(url,path,...) {
    body <- if(grepl("tile2",url)) f$body[-32] else f$body
    writeBin(body,path);download_response(length(body))
  })
})

test_that("size, response, signature, source and path guards fail closed", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  expect_true(dir.exists(prepare_stream_dem_download(f$selection,f$destination,list(file_bytes=16))))
  expect_error(prepare_stream_dem_download(f$selection,f$destination,list(idle_seconds=0)),"positive")
  path <- file.path(f$dir,"body");writeBin(f$body,path)
  expect_error(.fg_dem_verify(path,32,download_response(status=302)),"HTTP 302")
  expect_error(.fg_dem_verify(path,32,download_response(31)),"Content-Length")
  expect_error(.fg_dem_verify(path,31,download_response()),"Catalog size")
  expect_identical(.fg_dem_verify(path,NA_real_,download_response(headers=""))$verification$http_length,"unknown")
  writeBin(charToRaw("<html>not a raster</html>"),path)
  expect_error(.fg_dem_verify(path,NA_real_,download_response(headers="")),"signature")
  r <- read_stream_dem_selection(f$selection); row <- r$files
  expect_match(.fg_dem_url(r,row),"tile1.tif",fixed=TRUE)
  for(url in c("https://example.org/tile.tif",sub("Survey/","Survey/../",row$download_url),
      sub("tile1","%2e%2e",row$download_url),paste0(row$download_url,"?secret=x"),
      sub("tile1.tif","archive.zip",row$download_url))) {
    row$download_url <- url;expect_error(.fg_dem_url(r,row),"Unsupported")
  }
  expect_error(.fg_dem_inside(tempdir(),f$dir),"outside")
})

test_that("large catalog sizes are admitted and refreshed evidence reuses stable source bytes", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  large <- f$inventory;large$files$size_bytes <- 60*1024^3
  large_path <- file.path(f$dir,"large.gpkg")
  write_stream_dem_selection(large,"tile1",large_path,"revision")
  expect_true(dir.exists(prepare_stream_dem_download(large_path,f$destination)))
  transfers <- 0L
  with_mocked_bindings({
    a <- prepare_stream_dem_download(f$selection,f$destination)
    expect_identical(run_stream_dem_download(a)$files$outcome,"DOWNLOADED")
    refreshed <- f$inventory;refreshed$collection$snapshot_id <- "refreshed"
    refreshed$files$title <- "Updated catalog title"
    path <- file.path(f$dir,"refreshed.gpkg")
    write_stream_dem_selection(refreshed,"tile1",path,"later-revision")
    b <- prepare_stream_dem_download(path,f$destination)
    expect_identical(run_stream_dem_download(b)$files$outcome,"REUSED")
    expect_equal(transfers,1L)
  },.fg_dem_transfer=function(url,path,...) {
    transfers <<- transfers+1L;writeBin(f$body,path);download_response()
  })
})

test_that("receipt failure never turns an orphan into a registered asset", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  a <- prepare_stream_dem_download(f$selection,f$destination)
  json <- .fg_dem_json
  with_mocked_bindings({
    expect_error(run_stream_dem_download(a),"Synthetic receipt failure")
    expect_false(read_stream_dem_download(a)$files$outcome %in% c("DOWNLOADED","REUSED"))
    expect_length(list.files(file.path(f$destination,"assets")),1L)
  },.fg_dem_transfer=function(url,path,...) {writeBin(f$body,path);download_response()},
  .fg_dem_json=function(x,path,immutable=TRUE) {
    if(basename(dirname(path))=="receipts") stop("Synthetic receipt failure")
    json(x,path,immutable)
  })
  cancel_stream_dem_download(a)
  expect_length(list.files(file.path(a,"incomplete")),0L)
  expect_length(list.files(file.path(f$destination,"assets")),1L)
})

test_that("curl streaming preserves cancellation and stalled-transfer recovery", {
  skip_if_not_installed("httpuv");skip_if_not_installed("callr")
  port <- httpuv::randomPort()
  server <- callr::r_bg(function(port) httpuv::runServer("127.0.0.1",port,
    list(call=function(req) {
      if(req$PATH_INFO=="/slow") Sys.sleep(2)
      list(status=200L,headers=list("Content-Type"="image/tiff"),
        body=as.raw(c(73,73,42,0,8,0,0,0,rep(0,24))))
    })),args=list(port=port),stdout=NULL,stderr=NULL,poll_connection=FALSE,supervise=TRUE)
  on.exit(server$kill())
  url <- paste0("http://127.0.0.1:",port,"/tile")
  ready <- FALSE
  for(i in seq_len(60)) {
    ready <- tryCatch({curl::curl_fetch_memory(url);TRUE},error=function(e) FALSE)
    if(ready) break
    Sys.sleep(.05)
  }
  expect_true(ready)
  path <- tempfile();on.exit(unlink(path),add=TRUE)
  limits <- .fg_dem_limits(list())
  response <- .fg_dem_transfer(url,path,limits,function() FALSE,function(...) NULL)
  expect_equal(.fg_dem_verify(path,32,response)$bytes,32)
  expect_error(.fg_dem_transfer(url,path,limits,function() TRUE,function(...) NULL),"cancelled")
  limits$idle_seconds <- .1
  expect_error(.fg_dem_transfer(sub("/tile$","/slow",url),path,limits,function() FALSE,function(...) NULL),"idle timeout")
  limits$idle_seconds <- 120;limits$file_seconds <- .1
  expect_equal(.fg_dem_transfer(sub("/tile$","/slow",url),path,limits,function() FALSE,function(...) NULL)$status_code,200)
})

test_that("old admission caps do not stop transfers and altered snapshots cannot execute", {
  f <- download_fixture(2);on.exit(unlink(f$dir,recursive=TRUE))
  f$inventory$files$size_bytes <- NA_real_
  selection <- file.path(f$dir,"unknown.gpkg")
  write_stream_dem_selection(f$inventory,f$inventory$files$file_id,selection,"revision")
  with_mocked_bindings({
    a <- prepare_stream_dem_download(selection,f$destination,list(attempt_bytes=32))
    x <- run_stream_dem_download(a)
    expect_identical(x$files$outcome,c("DOWNLOADED","DOWNLOADED"))
    expect_identical(jsonlite::read_json(file.path(a,"finished.json"))$outcome,"FINISHED")
    b <- prepare_stream_dem_download(selection,f$destination)
    writeBin(as.raw(1),file.path(b,"selection.gpkg"))
    expect_error(run_stream_dem_download(b),"checksum")
  },.fg_dem_transfer=function(url,path,limits,cancelled,progress) {
    writeBin(f$body,path);progress(32,NA_real_);download_response(headers="")
  })
})

test_that("linked asset directories outside the study are rejected", {
  f <- download_fixture();on.exit(unlink(f$dir,recursive=TRUE))
  outside <- tempfile();dir.create(outside);on.exit(unlink(outside,recursive=TRUE),add=TRUE)
  dir.create(f$destination)
  link <- file.path(f$destination,"assets")
  linked <- suppressWarnings(file.symlink(outside,link))
  skip_if_not(linked,"Directory symlinks unavailable to this account")
  on.exit(unlink(link),add=TRUE,after=FALSE)
  expect_error(prepare_stream_dem_download(f$selection,f$destination),"outside")
  expect_length(list.files(outside),0L)
})
