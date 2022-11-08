library(fluvgeo)
context("arc2sp")

skip_if_no_arc <- function() {
  skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

fc_path <- file.path(system.file("extdata", "testing_data.gdb",
                                 package = "fluvgeo"), "riffle_channel")

double_backslash_path <- gsub("/", "\\\\", fc_path)

bad_path <- file.path("c:", "bogus", "path")


test_that("arc2sp works!", {
  skip_if_no_arc()
  load_libraries()
  fc_sp <- arc2sp(fc_path = fc_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})

test_that("double backslash path", {
  skip_if_no_arc()
  load_libraries()
  fc_sp <- arc2sp(fc_path = double_backslash_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})

test_that("bad path", {
  skip_if_no_arc()
  load_libraries()
  expect_error(arc2sp(fc_path = bad_path))
})

test_that("verify CRS", {
  skip_if_no_arc()
  load_libraries()
  fc_sp <- arc2sp(fc_path = fc_path)
  sp_known_crs <- sp::CRS(SRS_string = "EPSG:26915")
  sp_output_crs <- fc_sp@proj4string
  expect_true(raster::compareCRS(sp_known_crs, sp_output_crs))
})
