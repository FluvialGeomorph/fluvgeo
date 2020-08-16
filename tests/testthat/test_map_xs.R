library(fluvgeo)
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
  library(tmap)
  library(arcgisbinding)
  arcgisbinding::arc.check_product()
}

# sf
# Get feature class test data
xs_fc        <- file.path(system.file("extdata", "testing_data.gdb",
                                      package = "fluvgeo"),
                          "riffle_channel")
banklines_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                      package = "fluvgeo"),
                          "banklines")
dem          <- file.path(system.file("extdata", "testing_raster.gdb",
                                      package = "fluvgeo"),
                          "dem_1m")

# Convert feature classes to sf objects
xs_sf        <- fluvgeo::fc2sf(xs_fc)
banklines_sf <- fluvgeo::fc2sf(banklines_fc)

# Set other parameters
xs_number <- 1
extent_factor <- 10

# sf
test_that("check map_xs with sf inputs", {
  skip_if_no_arc()
  load_libraries()

  # Create map
  xs_map_sf <- map_xs(cross_section = xs_sf,
                      xs_number = xs_number,
                      dem = dem,
                      banklines = banklines_sf,
                      extent_factor = extent_factor)
  print(xs_map_sf)

  expect_true("tmap" %in% class(xs_map_sf))
  expect_error(print(xs_map_sf), NA)
})
