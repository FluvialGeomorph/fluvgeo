load_libraries <- function() {
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
  temp_gdb_path <- file.path(temp_folder, "testing_data.gdb")
  return(temp_gdb_path)
}

# Get sf objects
line_1_sf  <- fluvgeo::sin_flowline_sf
line_2_sf  <- fluvgeo::sin_riffle_channel_sf

test_that("sf: check output gdb table exists 1", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  sf_object <- line_1_sf
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 1)
  table_path <- file.path(temp_gdb_path, paste0("temp_line_",
                                             round(stats::runif(1, 1, 10000),
                                                   digits = 0)))
  print(table_path)
  sf2arc_table(sf_object = sf_object, table_path = table_path)
  arcobj <- arcgisbinding::arc.open(table_path)
  expect_true(exists("arcobj"))
  expect_true(arcobj@path == table_path)
})

test_that("sf: check output gdb table exists 2", {
  testthat::skip_if_not_installed("arcgisbinding")
  load_libraries()
  sf_object <- line_2_sf
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 2)
  table_path <- file.path(temp_gdb_path, paste0("temp_line_",
                                                round(stats::runif(1, 1, 10000),
                                                      digits = 0)))
  print(table_path)
  sf2arc_table(sf_object = sf_object, table_path = table_path)
  arcobj <- arcgisbinding::arc.open(table_path)
  expect_true(exists("arcobj"))
  expect_true(arcobj@path == table_path)
})
