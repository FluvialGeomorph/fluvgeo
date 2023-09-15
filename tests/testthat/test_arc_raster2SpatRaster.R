# The `arc_raster2SpatRaster` function needs `arcgisbinding` to read a file
# geodatabase raster.

# Helper functions
skip_if_no_arc <- function() {
  testthat::skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(arcgisbinding)
  arcgisbinding::arc.check_product()
}

raster_path <- file.path(system.file("extdata", "testing_raster.gdb",
                                     package = "fluvgeo"),
                         "dem_1m")
tif_path    <- file.path(system.file("extdata", "dem_1m.tif",
                                     package = "fluvgeo"))

test_that("convert ESRI GDB raster to SpatRaster", {
  skip_if_no_arc()
  load_libraries()

  gdb_raster <- arc_raster2SpatRaster(raster_path = raster_path)
  #plot(gdb_raster)

  expect_true("SpatRaster" %in% class(gdb_raster))
  expect_equal(dim(gdb_raster), c(1887, 1142, 1))
  expect_true(terra::same.crs(gdb_raster, terra::rast(tif_path)))
})

test_that("convert .tif raster to SpatRaster", {
  skip_if_no_arc()
  load_libraries()

  tif_raster <- arc_raster2SpatRaster(raster_path = tif_path)
  #plot(tif_raster)

  expect_true("SpatRaster" %in% class(tif_raster))
  expect_equal(dim(tif_raster), c(1887, 1142, 1))
  expect_true(terra::same.crs(tif_raster, terra::rast(tif_path)))
})
