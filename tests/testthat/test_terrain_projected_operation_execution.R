# Installed-stack qualification of projection + resampling + unit conversion.
# No datum change is involved, and this is not a general execution API.
test_that("an explicit projected operation agrees with a horizontal raster control", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture),"Provide the retained real DEM seam windows")
  source <- readRDS(fixture)$sources[1L]
  before <- tools::md5sum(source)
  root <- withr::local_tempdir()
  x <- terra::rast(source)
  # Control: remove the vertical declaration from the in-memory raster handle,
  # then use terra's ordinary 2D projection with metre samples. Source bytes stay
  # unchanged. Float64 control storage isolates final Float32 rounding.
  terra::crs(x) <- sf::st_crs("EPSG:6344")$wkt
  control <- terra::project(x,"EPSG:6345",res=2,method="bilinear",
    filename=file.path(root,"control.tif"),wopt=list(datatype="FLT8S"))
  catalog <- terrain_transform_candidates("EPSG:6344+5703","EPSG:6345+8228",
    c(-91.1,41.5,-90.8,41.7))
  candidates <- Filter(function(c) isTRUE(c$selectable),catalog$candidates)
  expect_length(candidates,1L)
  operation <- candidates[[1L]]
  vrt <- file.path(root,"operation.vrt")
  options <- c("-of","VRT","-s_srs",catalog$source$wkt,"-t_srs",catalog$target$wkt,
    "-ct",operation$definition,"-vshift","-r","bilinear","-ot","Float32","-et","0","-ovr","NONE",
    "-te",as.character(as.vector(terra::ext(control))[c(1,3,2,4)]),
    "-ts",as.character(c(terra::ncol(control),terra::nrow(control))),"-dstnodata","nan")
  expect_true(sf::gdal_utils("warp",source,vrt,options=options,
    config_options=c(PROJ_NETWORK="OFF",GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE"),quiet=TRUE))
  result <- terra::rast(vrt)
  terra::crs(result) <- catalog$target$wkt
  terra::units(result) <- "ft"
  output <- file.path(root,"terrain.tif")
  terra::writeRaster(result,output,wopt=list(datatype="FLT4S",
    gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER")))
  out <- terra::rast(output)
  a <- terra::values(control);b <- terra::values(out)
  expect_true(any(is.finite(b)))
  expect_identical(is.na(b),is.na(a))
  expect_lt(max(abs(b-a/0.3048),na.rm=TRUE),1e-4)
  expect_true(terra::compareGeom(control,out,crs=FALSE))
  expect_equal(terra::res(out),c(2,2))
  metadata <- .fg_vertical_observe(output,internal=TRUE)
  expect_true(metadata$band_unit %in% c("ft","foot"))
  expect_true(isTRUE(sf::st_crs(metadata$wkt)==sf::st_crs(catalog$target$wkt)))
  expect_identical(tools::md5sum(source),before)
})
