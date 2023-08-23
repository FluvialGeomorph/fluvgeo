library(fluvgeo)
context("arc2sp")

load_libraries <- function() {
  library(sp)
  library(sf)
  library(arcgisbinding)
  arc.check_product()
}

fc_path <- file.path(system.file("extdata", "testing_data.gdb",
                                 package = "fluvgeo"),
                     "feature_dataset/riffle_channel")

double_backslash_path <- gsub("/", "\\\\", fc_path)

bad_path <- file.path("c:", "bogus", "path")


test_that("data type the same?", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sp <- arc2sp(fc_path = fc_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})

test_that("double backslash path", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sp <- arc2sp(fc_path = double_backslash_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})

test_that("bad path", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  expect_error(arc2sp(fc_path = bad_path))
})

test_that("verify CRS via EPSG code", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sp <- arc2sp(fc_path = fc_path)
  sp_known_crs <- sp::CRS(SRS_string = "EPSG:26915")
  sp_output_crs <- fc_sp@proj4string
  expect_true(raster::compareCRS(sp_known_crs, sp_output_crs))
})

test_that("verify CRS via WKT string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sp  <- arc2sp(fc_path = fc_path)
  fc_wkt <- get_arc_wkt(fc_path)
  fc_crs <- sp::CRS(SRS_string = fc_wkt)
  sp_crs <- fc_sp@proj4string
  expect_true(raster::compareCRS(fc_crs, sp_crs))
})
