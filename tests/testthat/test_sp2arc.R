library(fluvgeo)
context("sp2arc")

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

double_backslash_path <- function(fc_path) {
  gsub("/", "\\\\", fc_path)
}

forward_slash_path <- function(fc_path) {
  gsub("\\\\", "/", fc_path)
}

create_temp_gdb <- function() {
  temp_folder <- forward_slash_path(tempdir())
  gdb_path <- file.path(system.file("extdata", "testing_data.gdb",
                                    package = "fluvgeo"))
  file.copy(from = gdb_path, to = temp_folder, recursive = TRUE)
  temp_gdb_path <- file.path(temp_folder, "testing_data.gdb")
  return(temp_gdb_path)
}

# Get sp objects
point_sp <- fluvgeo::sin_features_sp
line_sp  <- fluvgeo::sin_flowline_sp


test_that("check output points gdb fc exists", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb()
  point_fc <- file.path(temp_gdb_path, paste0("temp_point",
                                              round(stats::runif(1, 1, 10000),
                                                    digits = 0)))
  sp2arc(sp_object = point_sp, fc_path = point_fc)
  arcobj <- arcgisbinding::arc.open(point_fc)
  expect_true(exists("arcobj"))
  unlink(temp_gdb_path, recursive = TRUE)
})

test_that("check output points shapefile exists", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_file <- create_shapefile()
  sp2arc(sp_object = point_sp, fc_path = temp_file)
  expect_true(file.exists(temp_file))
  #delete_shapefile(temp_file)
})

test_that("check output lines shapefile exists", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_file <- create_shapefile()
  sp2arc(sp_object = line_sp, fc_path = temp_file)
  expect_true(file.exists(temp_file))
  #delete_shapefile(temp_file)
})

test_that("verify fc CRS via WKT string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb()
  point_fc <- file.path(temp_gdb_path, paste0("temp_point",
                                              round(stats::runif(1, 1, 10000),
                                                    digits = 0)))
  sp2arc(sp_object = point_sp, fc_path = point_fc)
  fc_wkt <- get_arc_wkt(point_fc)
  unlink(temp_gdb_path, recursive = TRUE)
})

test_that("verify shapefile CRS via WKT string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_file <- create_shapefile()
  sp2arc(sp_object = point_sp, fc_path = temp_file)
  shapefile_wkt <- get_arc_wkt(temp_file)
  shapefile_crs <- sp::CRS(SRS_string = shapefile_wkt)
  sp_crs <- point_sp@proj4string
  expect_true(raster::compareCRS(shapefile_crs, sp_crs))
  #delete_shapefile(temp_file)
})

test_that("check invalid input data", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_file <- create_shapefile()
  expect_error(sp2arc("7", temp_file))
  #delete_shapefile(temp_file)
})
