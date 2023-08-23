library(fluvgeo)
context("arc2sf")

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
  fc_sf <- arc2sf(fc_path = fc_path)
  expect_equal(class(fc_sf)[1], "sf")
})

test_that("double backslash path", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sf <- arc2sf(fc_path = double_backslash_path)
  expect_equal(class(fc_sf)[1], "sf")
})

test_that("bad path", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  expect_error(arc2sf(fc_path = bad_path))
})

test_that("verify CRS via EPSG code", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sf <- arc2sf(fc_path = fc_path)
  sf_known_crs <- sf::st_crs("EPSG:26915")
  sf_output_crs <- st_crs(fc_sf)
  expect_true(raster::compareCRS(sf_known_crs, sf_output_crs))
})

test_that("verify CRS via WKT string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  fc_sf  <- arc2sf(fc_path = fc_path)
  fc_wkt <- get_arc_wkt(fc_path)
  fc_crs <- sf::st_crs(fc_wkt)
  sf_crs <- st_crs(fc_sf)
  expect_true(raster::compareCRS(fc_crs, sf_crs))
})
