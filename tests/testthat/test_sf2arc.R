library(fluvgeo)
context("sf2arc")

load_libraries <- function() {
  library(sf)
  library(arcgisbinding)
  arc.check_product()
}

forward_slash_path <- function(fc_path) {
  gsub("\\\\", "/", fc_path)
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

# Get sf objects
point_sf <- fluvgeo::sin_features_sf
line_sf  <- fluvgeo::sin_flowline_sf

# testing variable
sf_object <- line_sf

test_that("check output points gdb fc exists", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 1)
  fc_path <- file.path(temp_gdb_path, paste0("temp_line",
                                             round(stats::runif(1, 1, 10000),
                                                   digits = 0)))
  print(fc_path)
  sf2arc(sf_object = line_sf, fc_path = fc_path)
  arcobj <- arcgisbinding::arc.open(fc_path)
  expect_true(exists("arcobj"))
  expect_true(arcobj@path == fc_path)
})
