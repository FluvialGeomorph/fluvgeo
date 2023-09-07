library(fluvgeo)
library(sf)
context("map_xs")

# The `map_xs` function needs `arcgisbinding` to read a file geodatabase raster.
# No other means currently exist to read file geodatabase rasters into R.

# Helper functions
skip_if_no_arc <- function() {
  testthat::skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(sf)
  library(raster)
  library(tmap)
  library(arcgisbinding)
  arcgisbinding::arc.check_product()
}

# fc to sf
cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_channel")
banklines_fc     <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/banklines")
dem_path        <- file.path(system.file("extdata", "testing_raster.gdb",
                                         package = "fluvgeo"),
                             "dem_1m")
# Get raster
# if(skip_if_no_arc()){
#   load_libraries()
#   dem <- fluvgeo::esri_raster2RasterLayer(raster_path = dem_path)
# }


cross_section <- fluvgeo::fc2sf(cross_section_fc)
banklines     <- fluvgeo::fc2sf(banklines_fc)

# Set other parameters
xs_number <- 1
extent_factor <- 1.2


# sf
cross_section_sf <- fluvgeo::sin_riffle_floodplain_dims_L3_sf
banklines_sf     <- fluvgeo::sin_banklines_sf

# rename sf inputs
xs_sf <- cross_section_sf
bl_sf <- banklines_sf



test_that("check map_xs with sf inputs", {
  skip_if_no_arc()
  load_libraries()
  dem <- fluvgeo::esri_raster2RasterLayer(raster_path = dem_path)
  # Create map
  xs_map_sf <- fluvgeo::map_xs(cross_section = cross_section,
                               xs_number = xs_number,
                               dem = dem,
                               banklines = banklines,
                               extent_factor = extent_factor)
  print(xs_map_sf)

  expect_true("tmap" %in% class(xs_map_sf))
  expect_error(print(xs_map_sf), NA)
})

test_that("check map_xs with no banklines", {
  skip_if_no_arc()
  load_libraries()
  dem <- fluvgeo::esri_raster2RasterLayer(raster_path = dem_path)
  # Create map
  xs_map_nb <- map_xs(cross_section = cross_section,
                      xs_number = xs_number,
                      dem = dem,
                      extent_factor = extent_factor)
  print(xs_map_nb)

  expect_true("tmap" %in% class(xs_map_nb))
  expect_error(print(xs_map_nb), NA)
})


test_that("check map_xs with different coordinate system inputs", {
  skip_if_no_arc()
  load_libraries()
  dem <- fluvgeo::esri_raster2RasterLayer(raster_path = dem_path)
  # Reproject to IL SP W USFT
  ilspwusft <- st_crs(3436)
  cross_section_il <- sf::st_transform(cross_section, crs = ilspwusft)

  # Create map
  xs_map_sf_il <- map_xs(cross_section = cross_section_il,
                         xs_number = xs_number,
                         dem = dem,
                         banklines = banklines,
                         extent_factor = extent_factor)
  print(xs_map_sf_il)

  expect_true("tmap" %in% class(xs_map_sf_il))
  expect_error(print(xs_map_sf_il), NA)
})

