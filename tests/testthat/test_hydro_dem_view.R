test_that("viewport display reports aggregation and preserves its source",{
  directory <- withr::local_tempdir()
  r <- terra::rast(nrows=20,ncols=20,xmin=500000,xmax=500020,
    ymin=4450000,ymax=4450020,crs="EPSG:6344")
  terra::values(r) <- seq_len(400);terra::units(r) <- "ft"
  source <- file.path(directory,"source.tif");terra::writeRaster(r,source)
  before <- tools::md5sum(source)
  b <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(c(
    xmin=500000,ymin=4450000,xmax=500020,ymax=4450020),crs=6344)),4326))
  bounds <- list(west=b[1],south=b[2],east=b[3],north=b[4])
  view <- prepare_hydro_dem_view(source,bounds,directory)
  expect_true(view$native)
  expect_equal(view$limits,c(1,400))
  expect_equal(view$resolution,c(1,1))
  expect_true(file.exists(view$hill))
  expect_equal(sf::st_crs(terra::crs(terra::rast(view$elevation)))$epsg,3857)
  overview_dir <- file.path(directory,"overview");dir.create(overview_dir)
  overview <- prepare_hydro_dem_view(source,bounds,overview_dir,pixels=25)
  expect_false(overview$native)
  expect_identical(tools::md5sum(source),before)
  expect_error(prepare_hydro_dem_view(source,list(west=0,south=0,east=-1,north=1),directory),"bounds")
  expect_true(prepare_hydro_dem_view(source,list(west=0,south=0,east=1,north=1),directory)$empty)
  world <- file.path(directory,"world");dir.create(world)
  expect_false(isTRUE(prepare_hydro_dem_view(source,list(west=-180,south=-85,east=180,north=85),world)$empty))
  wrapped <- bounds;wrapped$west <- wrapped$west+360;wrapped$east <- wrapped$east+360
  other <- file.path(directory,"wrapped");dir.create(other)
  expect_equal(prepare_hydro_dem_view(source,wrapped,other)$limits,view$limits)
  direct <- file.path(directory,"direct");dir.create(direct)
  absent_cache <- file.path(directory,"not-built")
  fast <- prepare_hydro_dem_view(source,bounds,direct,cache_directory=absent_cache,build_cache=FALSE)
  expect_true(dir.exists(file.path(absent_cache,"views")))
  expect_true(fast$native)
  expect_equal(terra::values(terra::rast(fast$elevation)),terra::values(terra::rast(view$elevation)))
  expect_true(any(is.finite(terra::values(terra::rast(fast$hill)))))
  expect_identical(tools::md5sum(source),before)
})

test_that("persistent display cache reuses and bounds prepared viewports",{
  directory <- withr::local_tempdir()
  cache <- file.path(directory,"cache")
  r <- terra::rast(nrows=20,ncols=20,xmin=500000,xmax=500020,
    ymin=4450000,ymax=4450020,crs="EPSG:6344")
  terra::values(r) <- seq_len(400)
  source <- file.path(directory,"source.tif");terra::writeRaster(r,source)
  b <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(c(
    xmin=500000,ymin=4450000,xmax=500020,ymax=4450020),crs=6344)),4326))
  bounds <- list(west=b[1],south=b[2],east=b[3],north=b[4])
  first_dir <- file.path(directory,"first");dir.create(first_dir)
  first <- prepare_hydro_dem_view(source,bounds,first_dir,
    cache_directory=cache,build_cache=FALSE)
  expect_match(first$elevation,"[\\\\/]views[\\\\/]",perl=TRUE)
  unlink(first_dir,recursive=TRUE)
  second_dir <- file.path(directory,"second");dir.create(second_dir)
  second <- prepare_hydro_dem_view(source,bounds,second_dir,
    cache_directory=cache,build_cache=FALSE)
  expect_identical(second$elevation,first$elevation)
  expect_true(file.exists(second$elevation))
  expect_equal(second$limits,first$limits)
})

test_that("producer cache follows immutable source identity across publication",{
  directory <- withr::local_tempdir();cache <- file.path(directory,"cache")
  r <- terra::rast(nrows=20,ncols=20,xmin=500000,xmax=500020,
    ymin=4450000,ymax=4450020,crs="EPSG:6344")
  terra::values(r) <- seq_len(400)
  staged <- file.path(directory,"staged.tif");terra::writeRaster(r,staged)
  prepared <- prepare_hydro_dem_display_cache(staged,cache,"content-sha")
  expect_true(all(file.exists(unlist(prepared))))
  published <- file.path(directory,"published.tif")
  expect_true(file.rename(staged,published))
  b <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(c(
    xmin=500000,ymin=4450000,xmax=500020,ymax=4450020),crs=6344)),4326))
  output <- file.path(directory,"view");dir.create(output)
  view <- prepare_hydro_dem_view(published,list(west=b[1],south=b[2],
    east=b[3],north=b[4]),output,cache_directory=cache,build_cache=FALSE,
    source_id="content-sha")
  expect_true(file.exists(view$elevation))
  expect_equal(view$limits,c(1,400))
})
