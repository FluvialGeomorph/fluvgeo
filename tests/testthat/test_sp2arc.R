library(fluvgeo)
context("sp2arc")

skip_if_no_arc <- function() {
  skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

delete_shapefile <- function(file_with_path) {
  shapefile_list <- list.files(dirname(file_with_path),
                               pattern = tools::file_path_sans_ext(basename(file_with_path)),
                                                  full.names = TRUE)
  file.remove(shapefile_list)
}

# sp object
fc <- fluvgeo::sin_flowline_sp

# Create a path to a temp file
temp_file <- tempfile("flowline", fileext = ".shp")

test_that("check if sp2arc works", {
  skip_if_no_arc()
  load_libraries()
  sp2arc(fc, temp_file)
  expect_true(file.exists(temp_file))
  delete_shapefile(temp_file)
})

