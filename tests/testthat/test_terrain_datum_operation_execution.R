# Opt-in qualification with real DEM pixels and official local NADCON5 grids.
# This is a horizontal realization shift with NAVD88 unit conversion, not a
# qualification of vertical datum shifts or the full Stream mosaic workflow.
test_that("a grid-backed datum pipeline shifts coordinates and preserves height semantics", {
  skip_if_not_installed("callr")
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  data <- Sys.getenv("FLUVGEO_TEST_PROJ_DATA")
  skip_if(!nzchar(fixture) || !nzchar(data),"Provide real DEM windows and isolated PROJ data")
  paths <- sf::sf_proj_search_paths()
  withr::defer(sf::sf_proj_search_paths(paths))
  sf::sf_proj_search_paths(data)
  catalog <- terrain_transform_candidates("EPSG:6344+5703","EPSG:26915+8228",
    c(-91.1,41.5,-90.8,41.7))
  candidates <- Filter(function(c) isTRUE(c$selectable),catalog$candidates)
  expect_length(candidates,1L)
  operation <- candidates[[1L]]
  expect_length(operation$grids,4L)
  expect_true(all(vapply(operation$grids,function(g) isTRUE(g$locally_verified) &&
    grepl("^[0-9a-f]{64}$",g$sha256),logical(1))))
  expect_match(operation$definition,"no_z_transform")
  source <- readRDS(fixture)$sources[1L]
  before <- tools::md5sum(source)
  x <- terra::rast(source)
  ids <- c(5000,10000,15000)
  xy <- terra::xyFromCell(x,ids)
  points <- sf::st_as_sf(data.frame(x=xy[,1],y=xy[,2],z=100),
    coords=c("x","y","z"),crs=catalog$source$wkt)
  shifted <- sf::st_transform(points,crs=catalog$target$wkt,pipeline=operation$definition)
  coordinates <- sf::st_coordinates(shifted)
  expect_gt(max(abs(coordinates[,1:2]-xy)),0.1)
  expect_equal(unname(coordinates[,3]),rep(100/0.3048,length(ids)),tolerance=1e-9)
  # Installed sf compares the forward definition against reverse candidates and
  # warns, while still executing the explicitly requested inverse. Check that
  # exact diagnostic and the numerical round trip, rather than hiding warnings.
  expect_warning(back <- sf::st_transform(shifted,crs=catalog$source$wkt,
    pipeline=operation$definition,reverse=TRUE),"pipeline not found in PROJ-suggested candidate transformations",fixed=TRUE)
  expect_lt(max(abs(sf::st_coordinates(back)-sf::st_coordinates(points))),1e-5)
  root <- withr::local_tempdir()
  vrt <- file.path(root,"operation.vrt")
  options <- c("-of","VRT","-s_srs",catalog$source$wkt,"-t_srs",catalog$target$wkt,
    "-ct",operation$definition,"-vshift","-r","bilinear","-ot","Float32","-et","0","-ovr","NONE",
    "-te",as.character(as.vector(terra::ext(x))[c(1,3,2,4)]),
    "-ts",as.character(c(terra::ncol(x),terra::nrow(x))),"-dstnodata","nan")
  expect_true(sf::gdal_utils("warp",source,vrt,options=options,
    config_options=c(PROJ_NETWORK="OFF",GDAL_PAM_ENABLED="NO"),quiet=TRUE))
  result <- terra::rast(vrt);terra::crs(result) <- catalog$target$wkt;terra::units(result) <- "ft"
  output <- file.path(root,"terrain.tif")
  terra::writeRaster(result,output,datatype="FLT4S")
  out <- terra::rast(output)
  q <- sf::st_as_sf(data.frame(x=xy[,1],y=xy[,2],z=0),coords=c("x","y","z"),crs=catalog$target$wkt)
  expect_warning(inverse <- sf::st_transform(q,crs=catalog$source$wkt,
    pipeline=operation$definition,reverse=TRUE),"pipeline not found in PROJ-suggested candidate transformations",fixed=TRUE)
  source_xy <- sf::st_coordinates(inverse)[,1:2]
  expected <- terra::extract(x,source_xy,method="bilinear")[[1L]]/0.3048
  actual <- as.vector(terra::values(out))[ids]
  expect_true(all(is.finite(actual)))
  expect_lt(max(abs(actual-expected)),1e-4)
  metadata <- .fg_vertical_observe(output,internal=TRUE)
  expect_true(metadata$band_unit %in% c("ft","foot"))
  expect_true(isTRUE(sf::st_crs(metadata$wkt)==sf::st_crs(catalog$target$wkt)))
  expect_identical(tools::md5sum(source),before)
  # PROJ may cache grids after a path change. Check missing resources in a fresh
  # process, as the app does for each worker, rather than trusting a warm context.
  absent <- callr::r(function(args,paths) {
    sf::sf_proj_search_paths(paths)
    do.call(fluvgeo::terrain_transform_candidates,args)
  },args=list(args=list(source_crs=catalog$source$wkt,target_crs=catalog$target$wkt,
    aoi=catalog$aoi),paths=paths),libpath=.libPaths())
  expect_false(any(vapply(absent$candidates,function(c) isTRUE(c$selectable) &&
    identical(c$description,operation$description),logical(1))))
})
