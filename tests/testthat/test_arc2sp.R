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

fc_path <- file.path(system.file("extdata", "testing_data.gdb", package = "fluvgeo"),
                     "riffle_channel")

test_that("arc2sp works!", {
  skip_if_no_arc()
  load_libraries()
  fc_sp <- arc2sp(fc_path = fc_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})
