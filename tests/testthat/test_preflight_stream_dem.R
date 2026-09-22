test_that("shared-anchor plans snap outward and child envelopes are exact subwindows", {
  area <- sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=-2.1,ymin=-1.1,xmax=3.1,ymax=4.1),crs=26915)))
  p <- .fg_dem_grid_plan(area,26915,1)
  expect_equal(p$extent,c(-3,-2,4,5));expect_equal(p$cells,49)
  expect_equal(p$mask_bytes,49);expect_equal(p$float32_bytes,196);expect_equal(p$float64_bytes,392)
  same <- .fg_dem_grid_plan(area,26915,1)
  expect_identical(p$index,same$index)
  other <- .fg_dem_grid_plan(area,26915,1.5)
  expect_equal(other$extent,c(-3,-1.5,4.5,4.5))
  child <- .fg_dem_grid_plan(area,26915,1,c(-1,-1,2,2))
  expect_equal(child$extent,c(-1,-1,2,2));expect_equal(child$cells,9)
  expect_error(.fg_dem_grid_plan(area,26915,1e-20),"dimensions")
  geographic <- sf::st_transform(area,4326)
  projected <- .fg_dem_grid_plan(geographic,26915,1)
  expect_equal(projected$extent,p$extent)
  expect_error(.fg_dem_grid_plan(sf::st_drop_geometry(area),26915,1),"polygon")
})

test_that("source spacing screen converts units without treating output spacing as source quality", {
  make_obs <- function(crs=26915,spacing=c(1,1),origin=c(500000,4500000)) {
    a <- list(wkt=sf::st_crs(crs)$wkt,band_unit="",grid=list(spacing=spacing,
      geotransform=c(origin[1],spacing[1],0,origin[2],0,-spacing[2]),horizontal_unit=sf::st_crs(crs)$units_gdal))
    list(default_reader=a,internal_compound=a)
  }
  a <- .fg_dem_source_screen(make_obs(),sf::st_crs(26915)$wkt,1)
  expect_match(a$alignment,"Aligned");expect_equal(a$grid_screen,"PASS")
  b <- .fg_dem_source_screen(make_obs(2276,c(2.5,2.5)),sf::st_crs(26915)$wkt,1)
  expect_equal(b$metres_x,2.5*1200/3937,tolerance=1e-12)
  expect_equal(b$grid_screen,"PASS");expect_match(b$alignment,"Reprojection")
  coarse <- .fg_dem_source_screen(make_obs(spacing=c(2,2)),sf::st_crs(26915)$wkt,.5)
  expect_match(coarse$issues,"exceeds the 1 m")
  expect_match(.fg_dem_source_screen(make_obs(4326,c(.00001,.00001)),sf::st_crs(26915)$wkt,1)$issues,"unresolved")
  rotated <- make_obs();rotated$default_reader$grid$geotransform[3] <- .1
  expect_match(.fg_dem_source_screen(rotated,sf::st_crs(26915)$wkt,1)$issues,"Rotated")
  expect_match(.fg_dem_source_screen(make_obs(spacing=c(.5,1)),sf::st_crs(26915)$wkt,1)$issues,"Anisotropic")
  expect_match(.fg_dem_source_screen(make_obs(origin=c(500000.5,4500000)),sf::st_crs(26915)$wkt,1)$alignment,"resampling")
  finer <- .fg_dem_source_screen(make_obs(spacing=c(.5,.5)),sf::st_crs(26915)$wkt,1)
  expect_equal(finer$grid_screen,"PASS");expect_match(finer$alignment,"resampling")
})

test_that("preflight binds current choices to real receipts and leaves every source unchanged", {
  root <- withr::local_tempdir(); request <- preflight_fixture(root)
  files <- list.files(root,recursive=TRUE,full.names=TRUE);before <- tools::md5sum(files)
  p <- do.call(preflight_stream_dem,request)
  expect_equal(p$grid_source_screen,"PASS");expect_false(p$processing_authorized)
  expect_equal(p$grids$columns,c(11,5,2));expect_equal(p$grids$cells,c(121,25,4))
  expect_equal(p$sources$metres_x,1);expect_match(p$sources$alignment,"Aligned")
  expect_equal(length(p$observations),1L);expect_identical(tools::md5sum(files),before)
  missing <- request;missing$sources$attempt <- NA_character_
  expect_equal(do.call(preflight_stream_dem,missing)$grid_source_screen,"BLOCKED")
  stale <- request; stale$selection <- file.path(root,"new-selection.gpkg");file.copy(request$selection,stale$selection)
  expect_error(do.call(preflight_stream_dem,stale),"changed")
  asset <- p$observations[[1]]$observation$path
  con <- file(asset,"ab");writeBin(as.raw(0),con);close(con)
  broken <- do.call(preflight_stream_dem,request)
  expect_equal(broken$grid_source_screen,"BLOCKED");expect_match(broken$sources$issues,"receipt")
})

test_that("missing Reach geometry remains an unresolved grid review", {
  request <- preflight_fixture(withr::local_tempdir(),reach_polygon=FALSE)
  p <- do.call(preflight_stream_dem,request)
  expect_equal(p$grid_source_screen,"REVIEW")
  expect_equal(p$grids$level,c("Study Area","Stream"))
  expect_true(any(grepl("Reach polygons are absent",p$notes)))
})
