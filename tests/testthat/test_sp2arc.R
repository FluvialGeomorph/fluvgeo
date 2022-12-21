library(fluvgeo)
context("sp2arc")

load_libraries <- function() {
  library(sp)
  library(arcgisbinding)
  arc.check_product()
}

forward_slash_path <- function(fc_path) {
  gsub("\\\\", "/", fc_path)
}

double_backslash_path <- function(fc_path) {
  gsub("/", "\\\\", fc_path)
}

temp_dir <- forward_slash_path(tempdir())

create_temp_gdb <- function(temp_folder_num = 1) {
  temp_folder <- file.path(temp_dir, temp_folder_num)
  dir.create(temp_folder, showWarnings = FALSE)
  gdb_path <- file.path(system.file("extdata", "testing_data.gdb",
                                    package = "fluvgeo"))
  file.copy(from = gdb_path, to = temp_folder, recursive = TRUE)
  temp_gdb_path <- file.path(temp_folder, "testing_data.gdb", "feature_dataset")
  return(temp_gdb_path)
}

# Get sp objects
point_sp <- fluvgeo::sin_features_sp
line_sp  <- fluvgeo::sin_flowline_sp

# testing variable
sp_object <- point_sp


test_that("check output points gdb fc exists", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 2)
  fc_path <- file.path(temp_gdb_path, paste0("temp_point",
                                              round(stats::runif(1, 1, 10000),
                                                    digits = 0)))
  print(fc_path)
  sp2arc(sp_object = point_sp, fc_path = fc_path)
  arcobj <- arcgisbinding::arc.open(fc_path)
  expect_true(exists("arcobj"))
  expect_true(arcobj@path == fc_path)
})

test_that("verify fc CRS via WKT string", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 1)
  fc_path <- file.path(temp_gdb_path, paste0("temp_point",
                                              round(stats::runif(1, 1, 10000),
                                                    digits = 0)))
  print(fc_path)
  sp2arc(sp_object = point_sp, fc_path = fc_path)
  sp_crs <- sp::CRS(SRS_string = sp::wkt(point_sp))
  fc_crs <- sp::CRS(SRS_string = get_arc_wkt(fc_path))
  expect_true(raster::compareCRS(sp_crs, fc_crs))
})

test_that("check invalid input data", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 3)
  fc_path <- file.path(temp_gdb_path, paste0("temp_point",
                                              round(stats::runif(1, 1, 10000),
                                                    digits = 0)))
  print(fc_path)
  expect_error(sp2arc("7", fc_path))
})
