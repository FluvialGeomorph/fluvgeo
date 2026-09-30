test_that("cutline minima lower only their footprint and preserve NoData",{
  d <- withr::local_tempdir()
  r <- terra::rast(nrows=5,ncols=5,xmin=500000,xmax=500005,ymin=4450000,ymax=4450005,crs="EPSG:6344")
  terra::values(r) <- seq_len(25);r[13] <- NA;terra::units(r) <- "ft"
  source <- file.path(d,"dem.tif");terra::writeRaster(r,source)
  before <- tools::md5sum(source)
  line <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(rbind(c(500000.5,4450002.5),c(500004.5,4450002.5))),crs=6344))
  result <- burn_hydro_cutlines(source,line,file.path(d,"hydro.tif"))
  out <- terra::rast(result$path)
  expected <- seq_len(25);expected[11:15] <- 11;expected[13] <- NA
  expect_equal(as.numeric(terra::values(out)),expected)
  expect_true(terra::compareGeom(r,out))
  expect_true(terra::units(out) %in% c("ft","foot"))
  expect_identical(tools::md5sum(source),before)
  expect_equal(result$widen_cells,0)
  expect_error(burn_hydro_cutlines(source,line,result$path),"already exists")
  second <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(rbind(c(500004.5,4450000.5),c(500004.5,4450004.5))),crs=6344))
  overlap <- burn_hydro_cutlines(source,rbind(line,second),file.path(d,"overlap.tif"))
  expected[c(5,10,20,25)] <- 5
  expect_equal(as.numeric(terra::values(terra::rast(overlap$path))),expected)
  expect_error(burn_hydro_cutlines(source,line[0,],file.path(d,"empty.tif")),"valid cutlines")
  from_map <- burn_hydro_cutlines(source,sf::st_transform(line,4326),file.path(d,"map-cut.tif"))
  expect_equal(as.numeric(terra::values(terra::rast(from_map$path))),as.numeric(terra::values(out)))
  expect_match(from_map$cutline_display_operation$definition,"proj=pipeline",fixed=TRUE)
  off <- line;sf::st_geometry(off) <- sf::st_geometry(off)+c(100,100);sf::st_crs(off) <- 6344
  partial <- burn_hydro_cutlines(source,rbind(off,line),file.path(d,"partial.tif"))
  expect_equal(partial$skipped_nodata_cutlines,1L)
  expect_equal(as.numeric(terra::values(terra::rast(partial$path))),as.numeric(terra::values(out)))
  expect_error(burn_hydro_cutlines(source,off,file.path(d,"outside.tif")),"Cutline\\(s\\) 1 cross only NoData")
})

test_that("a small interior cut preserves the full surrounding DEM",{
  d <- withr::local_tempdir()
  r <- terra::rast(nrows=20,ncols=20,xmin=500000,xmax=500020,
    ymin=4450000,ymax=4450020,crs="EPSG:6344")
  expected <- seq_len(400);expected[1] <- NA
  terra::values(r) <- expected;terra::units(r) <- "ft"
  source <- file.path(d,"source.tif");terra::writeRaster(r,source)
  line <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(rbind(
    c(500009.5,4450010.5),c(500011.5,4450010.5))),crs=6344))
  result <- burn_hydro_cutlines(source,line,file.path(d,"hydro.tif"))
  expected[190:192] <- 190
  out <- terra::rast(result$path)
  expect_equal(as.numeric(terra::values(out)),expected)
  expect_true(terra::compareGeom(r,out))
  expect_true(terra::units(out) %in% c("ft","foot"))
})
