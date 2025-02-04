
test_that("convert geodatabase raster to SpatRast", {
  raster_path <- file.path(system.file("extdata", "testing_raster.gdb",
                                     package = "fluvgeo"),
                         "dem_1m")

  raster <- gdb_raster2SpatRast(raster_path)
  expect_true("SpatRaster" %in% class(raster))
  expect_true("dem_1m" %in% names(raster))
  expect_equal(1887, terra::nrow(raster))
  expect_equal(1142, terra::ncol(raster))
  expect_equal(1,    terra::nlyr(raster))
  expect_equal(26915, sf::st_crs(raster)$epsg)
})

test_that("convert file raster to SpatRast", {
  raster_path <- file.path(system.file("extdata", "dem_1m.tif",
                                       package = "fluvgeo"))

  raster <- gdb_raster2SpatRast(raster_path)
  expect_true("SpatRaster" %in% class(raster))
  expect_true("Band_1" %in% names(raster))
  expect_equal(1887, terra::nrow(raster))
  expect_equal(1142, terra::ncol(raster))
  expect_equal(1,    terra::nlyr(raster))
  expect_equal(26915, sf::st_crs(raster)$epsg)
})

