library(fluvgeo)
context("map_xs")

skip_if_no_arc <- function() {
  skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

# Use the fluvgeo::sin_riffle_floodplain_sp SpatialLinesDataFrame
sin_riffle_channel_sp <- fluvgeo::sin_riffle_channel_sp

# Use the DEM in the `testing_raster.gdb`
gdb_path <- system.file("extdata", "testing_raster.gdb", package = "fluvgeo")
dem_path <- file.path(gdb_path, "dem_1m")

# Use the fluvgeo::sin_banklines_sp SpatialLinesDataFrame
sin_banklines_sp <- fluvgeo::sin_banklines_sp

test_that("check map_xs", {
  skip_if_no_arc()
  load_libraries()
  xs_map_1 <- map_xs(sin_riffle_channel_sp,
                     xs_number = 1,
                     dem = dem_path,
                     banklines = sin_banklines_sp)
  expect_true("tmap" %in% class(xs_map_1))
  expect_error(print(xs_map_1), NA)
})
