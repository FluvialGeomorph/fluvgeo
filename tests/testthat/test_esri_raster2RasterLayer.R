library(fluvgeo)
library(raster)
library(rgdal)
library(sp)
context("esri_raster2RasterLayer")

# The `map_xs` function needs `arcgisbinding` to read a file geodatabase raster.
# No other means currently exist to read file geodatabase rasters into R.

# Helper functions
skip_if_no_arc <- function() {
  testthat::skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(fluvgeo)
  library(raster)
  library(rgdal)
  library(sp)
  library(arcgisbinding)
  arcgisbinding::arc.check_product()
}

raster_path <- file.path(system.file("extdata", "testing_raster.gdb",
                                     package = "fluvgeo"),
                         "dem_1m")
tif_path    <- file.path(system.file("extdata", "dem_1m.tif",
                                     package = "fluvgeo"))

test_that("convert ESRI GDB raster to RasterLayer", {
  skip_if_no_arc()
  load_libraries()

  # Create map
  esri_raster <- esri_raster2RasterLayer(raster_path = raster_path)
  #plot(esri_raster)

  expect_true("RasterLayer" %in% class(esri_raster))
})

test_that("convert .tif raster to RasterLayer", {
  skip_if_no_arc()
  load_libraries()

  # Create map
  tif_raster <- esri_raster2RasterLayer(raster_path = tif_path)
  #plot(tif_raster)

  expect_true("RasterLayer" %in% class(tif_raster))
})
