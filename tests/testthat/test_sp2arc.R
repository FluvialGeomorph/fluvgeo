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

create_shapefile <- function() {
  temp_file <- tempfile("flowline", fileext = ".shp")
}

delete_shapefile <- function(file_with_path) {
  file_pattern <- tools::file_path_sans_ext(basename(file_with_path))
  shapefile_list <- list.files(dirname(file_with_path),
                               pattern = file_pattern,
                               full.names = TRUE)
  file.remove(shapefile_list)
}

# Get sp objects
point_sp <- fluvgeo::sin_features_sp
line_sp  <- fluvgeo::sin_flowline_sp


test_that("check points", {
  skip_if_no_arc()
  load_libraries()
  temp_file <- create_shapefile()
  sp2arc(sp_object = point_sp, fc_path = temp_file)
  expect_true(file.exists(temp_file))
  delete_shapefile(temp_file)
})

test_that("check lines", {
  skip_if_no_arc()
  load_libraries()
  temp_file <- create_shapefile()
  sp2arc(sp_object = line_sp, fc_path = temp_file)
  expect_true(file.exists(temp_file))
  delete_shapefile(temp_file)
})

test_that("check invalid input data", {
  skip_if_no_arc()
  load_libraries()
  temp_file <- create_shapefile()
  expect_error(sp2arc("7", temp_file))
  delete_shapefile(temp_file)
})
