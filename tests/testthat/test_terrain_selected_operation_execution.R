# Installed-stack qualification, not a general datum-transformation API.
# The opt-in source is a retained small actual DEM window, never a whole Stream.
test_that("an explicit unit-conversion pipeline produces consistent raster evidence", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture),"Provide the retained real DEM seam windows")
  source <- readRDS(fixture)$sources[1L]
  before <- tools::md5sum(source)
  x <- terra::rast(source)
  catalog <- terrain_transform_candidates("EPSG:6344+5703","EPSG:6344+8228",
    c(-91.1,41.5,-90.8,41.7))
  candidates <- Filter(function(c) isTRUE(c$selectable),catalog$candidates)
  expect_length(candidates,1L)
  # Explicit test choice of the exact catalog pipeline; no analyst choice is
  # inferred. This pair changes units only and requires no datum decision.
  operation <- candidates[[1L]]
  root <- withr::local_tempdir()
  vrt <- file.path(root,"operation.vrt")
  options <- c("-of","VRT","-s_srs",catalog$source$wkt,"-t_srs",catalog$target$wkt,
    "-ct",operation$definition,"-vshift","-r","near","-ot","Float32","-et","0","-ovr","NONE",
    "-te",as.character(as.vector(terra::ext(x))[c(1,3,2,4)]),
    "-ts",as.character(c(terra::ncol(x),terra::nrow(x))),"-dstnodata","nan")
  expect_true(sf::gdal_utils("warp",source,vrt,options=options,
    config_options=c(PROJ_NETWORK="OFF",GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE"),quiet=TRUE))
  # GDAL may retain the source band-unit label after converting the samples.
  # Materialize the virtual operation once, explicitly setting output metadata.
  result <- terra::rast(vrt)
  terra::crs(result) <- catalog$target$wkt
  terra::units(result) <- "ft"
  output <- file.path(root,"terrain.tif")
  terra::writeRaster(result,output,wopt=list(datatype="FLT4S",
    gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER")))
  out <- terra::rast(output)
  # Bounded fixture samples are allowed; production uses file-backed operations.
  a <- terra::values(x);b <- terra::values(out)
  expect_identical(is.na(b),is.na(a))
  expect_lt(max(abs(b-a/0.3048),na.rm=TRUE),1e-4)
  expect_true(terra::compareGeom(x,out,crs=FALSE))
  metadata <- .fg_vertical_observe(output,internal=TRUE)
  expect_true(metadata$band_unit %in% c("ft","foot"))
  expect_true(isTRUE(sf::st_crs(metadata$wkt)==sf::st_crs(catalog$target$wkt)))
  expect_identical(tools::md5sum(source),before)
})
