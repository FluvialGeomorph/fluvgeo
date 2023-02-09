library(fluvgeo)
context("get_arc_wkt")

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

fc_path <- file.path(system.file("extdata", "testing_data.gdb",
                                 package = "fluvgeo"),
                     "feature_dataset/riffle_channel")


test_that("check WKT is string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  arc_wkt <- get_arc_wkt(fc_path)
  expect_true(is.character(arc_wkt))
})

test_that("check WKID is integer", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  arc_wkid <- get_arc_wkt(fc_path, WKID = TRUE)
  expect_true(is.integer(arc_wkid))
})
