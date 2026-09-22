horizontal_fixture <- function(root,compound=FALSE,constant=FALSE,datatype="FLT4S") {
  r <- terra::rast(nrows=4,ncols=4,xmin=500000,xmax=500004,ymin=4500000,ymax=4500004,crs="EPSG:26915")
  terra::values(r) <- if(constant) rep(123.123456789,16) else c(0,-1.123456789,NA,4:16)
  terra::units(r) <- "ft"
  raw <- file.path(root,"raw.tif");terra::writeRaster(r,raw,datatype=datatype)
  source <- file.path(root,"source.tif")
  sf::gdal_utils("translate",raw,source,options=c("-of","GTiff","-a_srs",
    if(compound) "EPSG:26915+6360" else "EPSG:26915"),quiet=TRUE)
  template <- file.path(root,"template.tif");terra::writeRaster(r,template,datatype="FLT8S")
  list(source=source,template=template,directory=file.path(root,"attempt"))
}

test_that("Float32 is the default and aligned source values survive exactly", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,compound=TRUE)
  original <- terra::values(terra::rast(args$source))
  m <- do.call(warp_terrain_horizontal,args)
  expect_identical(m$output_type,"Float32")
  expect_identical(m$working_type,"Float64")
  expect_identical(terra::datatype(terra::rast(file.path(args$directory,"terrain.tif"))),"FLT4S")
  expect_equal(terra::values(terra::rast(file.path(args$directory,"terrain.tif"))),original,tolerance=0)
  expect_equal(m$required_bytes,8*16+256*1024^2)
})

test_that("Float32 storage rounds higher precision samples and refuses overflow", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,datatype="FLT8S")
  r <- terra::rast(args$source);terra::values(r) <- c(0,-0.1,NA,rep(1000.1,13))
  terra::writeRaster(r,args$source,overwrite=TRUE,datatype="FLT8S")
  m <- do.call(warp_terrain_horizontal,args)
  actual <- as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif"))))
  expect_equal(actual,c(0,-0.10000000149011612,NA,rep(1000.0999755859375,13)),tolerance=0)
  expect_true(m$aligned_samples_verified)
  args$directory <- file.path(root,"overflow")
  terra::values(r) <- 1e39;terra::writeRaster(r,args$source,overwrite=TRUE,datatype="FLT8S")
  expect_error(do.call(warp_terrain_horizontal,args),"exceed Float32")
  expect_false(dir.exists(args$directory))
  expect_error(do.call(warp_terrain_horizontal,c(args,list(output_type="Int16"))),"arg")
})

test_that("aligned Float64 samples and NoData survive compound horizontal processing exactly", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,compound=TRUE,datatype="FLT8S")
  args$output_type <- "Float64"
  before <- tools::md5sum(c(args$source,args$template))
  m <- do.call(warp_terrain_horizontal,args)
  expect_identical(m$resampling,"near");expect_false(m$scientific_acceptance)
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),c(0,-1.123456789,NA,4:16),tolerance=0)
  expect_equal(m$output$pixels$valid_cells,15)
  expect_identical(m$source$internal_compound$projjson$type,"CompoundCRS")
  expect_identical(m$output$observation$internal_compound$projjson$type,"ProjectedCRS")
  expect_identical(m$output$observation$internal_compound$band_unit,"US survey foot")
  expect_true("-novshift" %in% m$options)
  expect_true(file.exists(file.path(args$directory,"verified.json")))
  expect_identical(tools::md5sum(c(args$source,args$template)),before)
  expect_error(do.call(warp_terrain_horizontal,args),"new horizontal")
})

test_that("bilinear alignment has independently calculated interior values", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  r <- terra::rast(args$source);xy <- terra::xyFromCell(r,1:16)
  terra::values(r) <- 2*(xy[,1]-500000)+3*(xy[,2]-4500000)
  terra::writeRaster(r,args$source,overwrite=TRUE,datatype="FLT8S")
  t <- terra::rast(nrows=2,ncols=2,xmin=500001.25,xmax=500003.25,ymin=4500001.25,ymax=4500003.25,crs="EPSG:26915")
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  m <- do.call(warp_terrain_horizontal,args)
  expect_identical(m$resampling,"bilinear")
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),c(11.75,13.75,8.75,10.75),tolerance=1e-10)
})

test_that("real same-reference reprojection preserves constant height and band units", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,compound=TRUE,constant=TRUE)
  center <- sf::st_sfc(sf::st_point(c(500002,4500002)),crs=26915)
  xy <- sf::st_coordinates(sf::st_transform(center,26916))[1,]
  t <- terra::rast(nrows=2,ncols=2,xmin=xy[1]-.5,xmax=xy[1]+.5,ymin=xy[2]-.5,ymax=xy[2]+.5,crs="EPSG:26916")
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  m <- do.call(warp_terrain_horizontal,args)
  expect_identical(m$resampling,"bilinear")
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),rep(terra::values(terra::rast(args$source))[1],4),tolerance=0)
  expect_identical(m$output$observation$internal_compound$band_unit,"US survey foot")
  expect_match(m$operation$definition,"pipeline")
})

test_that("unqualified source operations and failed writes cannot produce verification", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  expect_error(do.call(warp_terrain_horizontal,c(args,list(max_cells=1))),"budget")
  expect_false(dir.exists(args$directory))
  sf::gdal_utils("translate",file.path(root,"raw.tif"),file.path(root,"scaled.tif"),options=c("-a_scale","2","-a_offset","10"),quiet=TRUE)
  scaled <- args;scaled$source <- file.path(root,"scaled.tif")
  expect_error(do.call(warp_terrain_horizontal,scaled),"scale/offset")
  file.create(paste0(args$source,".ovr"))
  expect_error(do.call(warp_terrain_horizontal,args),"sidecars")
  unlink(paste0(args$source,".ovr"))
  different <- terra::rast(args$template);terra::crs(different) <- "EPSG:32615"
  terra::values(different) <- 1;terra::writeRaster(different,args$template,overwrite=TRUE)
  expect_error(do.call(warp_terrain_horizontal,args),"static geodetic|Geodetic reference")
  expect_false(file.exists(file.path(args$directory,"verified.json")))
})

test_that("horizontal survey-foot axes do not convert source elevation samples", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,compound=TRUE,constant=TRUE)
  j <- .fg_horizontal_info(args$template)$info$stac[["proj:projjson"]]
  j$id <- NULL;j$name <- "Synthetic NAD83 UTM 15 survey-foot axes"
  for(i in 1:2) j$coordinate_system$axis[[i]]$unit <- list(type="LinearUnit",name="US survey foot",conversion_factor=1200/3937)
  wkt <- sf::st_crs(as.character(jsonlite::toJSON(j,auto_unbox=TRUE,digits=NA)))$wkt
  center <- sf::st_transform(sf::st_sfc(sf::st_point(c(500002,4500002)),crs=26915),wkt)
  xy <- sf::st_coordinates(center)[1,]
  t <- terra::rast(nrows=2,ncols=2,xmin=xy[1]-2.5,xmax=xy[1]+2.5,ymin=xy[2]-2.5,ymax=xy[2]+2.5,crs=wkt)
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  m <- do.call(warp_terrain_horizontal,args)
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),rep(terra::values(terra::rast(args$source))[1],4),tolerance=0)
  expect_match(m$operation$definition,"unitconvert")
  expect_match(m$output$observation$internal_compound$grid$horizontal_unit,"foot")
})

test_that("failed pixel verification leaves an unpublished attempt", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  scan <- .fg_horizontal_scan;calls <- 0L
  with_mocked_bindings(expect_error(do.call(warp_terrain_horizontal,args),"unreadable pixels"),
    .fg_horizontal_scan=function(path) {calls <<- calls+1L;if(calls==2L) stop("unreadable pixels");scan(path)})
  expect_true(dir.exists(args$directory))
  expect_false(file.exists(file.path(args$directory,"verified.json")))
  x <- .fg_horizontal_info(args$source)
  x$info$stac[["proj:projjson"]]$base_crs$datum$type <- "DynamicGeodeticReferenceFrame"
  expect_error(.fg_horizontal_crs(x),"Dynamic")
  x$info$stac[["proj:projjson"]]$type <- "BoundCRS"
  x$info$stac[["proj:projjson"]]$base_crs$datum$type <- "GeodeticReferenceFrame"
  expect_error(.fg_horizontal_crs(x),"projected 2D")
})

test_that("the installed GDAL control converts vertical units but the guarded path does not", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,compound=TRUE,constant=TRUE)
  control <- file.path(root,"default.tif")
  sf::gdal_utils("warp",args$source,control,options=c("-t_srs","EPSG:26915","-r","near","-ot","Float64"),
    config_options=c(GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE",PROJ_NETWORK="OFF"),quiet=TRUE)
  values <- as.vector(terra::values(terra::rast(control)))
  original <- as.vector(terra::values(terra::rast(args$source)))
  expect_equal(values,original*1200/3937,tolerance=1e-12)
  expect_false(isTRUE(all.equal(values,original)))
  do.call(warp_terrain_horizontal,args)
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),original,tolerance=0)
})

test_that("changed input bytes and exhausted storage cannot publish an attempt", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  with_mocked_bindings(expect_error(do.call(warp_terrain_horizontal,args),"disk space"),
    ps_disk_usage=function(...) data.frame(available=0),.package="ps")
  expect_false(dir.exists(args$directory))
  scan <- .fg_horizontal_scan;calls <- 0L
  with_mocked_bindings(expect_error(do.call(warp_terrain_horizontal,args),"inputs changed"),
    .fg_horizontal_scan=function(path) {
      result <- scan(path);calls <<- calls+1L
      if(calls==2L) {con <- file(args$source,"ab");writeBin(as.raw(0),con);close(con)}
      result
    })
  expect_false(file.exists(file.path(args$directory,"verified.json")))
})

test_that("aligned cropped and extended windows match original samples and NoData", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  t <- terra::rast(nrows=6,ncols=6,xmin=499999,xmax=500005,ymin=4499999,ymax=4500005,crs="EPSG:26915")
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  m <- do.call(warp_terrain_horizontal,args)
  expected <- matrix(NA_real_,6,6);expected[2:5,2:5] <- matrix(terra::values(terra::rast(args$source)),4,byrow=TRUE)
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),as.vector(t(expected)),tolerance=0)
  expect_true(m$aligned_samples_verified)
  t <- terra::rast(nrows=2,ncols=2,xmin=500001,xmax=500003,ymin=4500001,ymax=4500003,crs="EPSG:26915")
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  args$directory <- file.path(root,"cropped");do.call(warp_terrain_horizontal,args)
  expect_equal(as.vector(terra::values(terra::rast(file.path(args$directory,"terrain.tif")))),c(6,7,10,11),tolerance=0)
})

test_that("bilinear output does not fill a source NoData center", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root,constant=TRUE)
  r <- terra::rast(args$source);v <- rep(10,16);v[10] <- NA;terra::values(r) <- v
  terra::writeRaster(r,args$source,overwrite=TRUE,datatype="FLT8S")
  t <- terra::rast(nrows=1,ncols=1,xmin=500001.25,xmax=500001.75,ymin=4500001.25,ymax=4500001.75,crs="EPSG:26915")
  terra::values(t) <- 1;terra::writeRaster(t,args$template,overwrite=TRUE)
  m <- do.call(warp_terrain_horizontal,args)
  expect_identical(m$resampling,"bilinear")
  expect_equal(m$output$pixels$valid_cells,0)
  expect_true(is.na(terra::values(terra::rast(file.path(args$directory,"terrain.tif"))))[1])
})

test_that("an explicitly stored coordinate epoch requires a separate workflow", {
  root <- withr::local_tempdir();args <- horizontal_fixture(root)
  epoch <- file.path(root,"epoch.tif")
  sf::gdal_utils("translate",args$source,epoch,
    options=c("-a_coord_epoch","2020","-co","GEOTIFF_VERSION=1.1"),quiet=TRUE)
  args$source <- epoch
  expect_error(do.call(warp_terrain_horizontal,args),"Coordinate-epoch")
})
