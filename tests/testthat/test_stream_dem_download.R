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
  },.fg_dem_transfer=function(url,path,limits,max_bytes,cancelled,progress) {
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
  expect_error(prepare_stream_dem_download(f$selection,f$destination,list(file_bytes=16)),"byte limit")
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

test_that("curl streaming enforces limits and cancellation using a local HTTP fixture", {
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
  response <- .fg_dem_transfer(url,path,limits,32,function() FALSE,function(...) NULL)
  expect_equal(.fg_dem_verify(path,32,response)$bytes,32)
  expect_error(.fg_dem_transfer(url,path,limits,16,function() FALSE,function(...) NULL),"byte limit")
  expect_lte(file.info(path)$size,16)
  expect_error(.fg_dem_transfer(url,path,limits,32,function() TRUE,function(...) NULL),"cancelled")
  limits$idle_seconds <- .1
  expect_error(.fg_dem_transfer(sub("/tile$","/slow",url),path,limits,32,function() FALSE,function(...) NULL),"idle timeout")
  limits$idle_seconds <- 120;limits$file_seconds <- .1
  expect_error(.fg_dem_transfer(sub("/tile$","/slow",url),path,limits,32,function() FALSE,function(...) NULL),"[Tt]imeout|[Tt]imed out")
})

test_that("unknown sizes cannot evade budgets and altered snapshots cannot execute", {
  f <- download_fixture(2);on.exit(unlink(f$dir,recursive=TRUE))
  f$inventory$files$size_bytes <- NA_real_
  selection <- file.path(f$dir,"unknown.gpkg")
  write_stream_dem_selection(f$inventory,f$inventory$files$file_id,selection,"revision")
  with_mocked_bindings({
    a <- prepare_stream_dem_download(selection,f$destination,list(attempt_bytes=32))
    x <- run_stream_dem_download(a)
    expect_identical(x$files$outcome,c("DOWNLOADED","NOT_STARTED"))
    expect_identical(jsonlite::read_json(file.path(a,"finished.json"))$outcome,"LIMIT_REACHED")
    b <- prepare_stream_dem_download(selection,f$destination)
    writeBin(as.raw(1),file.path(b,"selection.gpkg"))
    expect_error(run_stream_dem_download(b),"checksum")
  },.fg_dem_transfer=function(url,path,limits,max_bytes,cancelled,progress) {
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
