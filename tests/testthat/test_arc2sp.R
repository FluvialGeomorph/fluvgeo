context("arc2sp")

skip_if_no_arc <- function() {
  skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  # Load libraries
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

load_data <- function() {
  # Path to an ESRI geodatabase feature class
  fc_path_in <- file.path(getwd(), "data-raw/test.gdb/riffle")
}

test_that("arc2sp works!", {
  skip_if_no_arc()
  load_libraries()
  in_fc_path <- load_data()
  fc_sp <- arc2sp(fc_path = in_fc_path)
  expect_equal(class(fc_sp)[1], "SpatialLinesDataFrame")
})
