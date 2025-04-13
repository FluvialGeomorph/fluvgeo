library(fluvgeo)
context("fc2sf")

forward_slash_path <- function(fc_path) {
  gsub("\\\\", "/", fc_path)
}

temp_dir <- forward_slash_path(tempdir())

create_temp_gdb <- function(temp_folder_num = 1) {
  temp_folder <- file.path(temp_dir, temp_folder_num)
  dir.create(temp_folder, showWarnings = FALSE)
  gdb_path <- file.path(system.file("extdata", "testing_data.gdb",
                                    package = "fluvgeodata"))
  file.copy(from = gdb_path, to = temp_folder, recursive = TRUE)
  temp_gdb_path <- file.path(temp_folder, "testing_data.gdb", "feature_dataset")
  return(temp_gdb_path)
}


test_that("check output points gdb fc exists", {
  temp_gdb_path <- create_temp_gdb(temp_folder_num = 1)
  fc_path <- file.path(temp_gdb_path, "flowline")
  print(fc_path)
  sf <- fc2sf(fc_path)
  expect_true(class(sf)[1] == "sf")
})
