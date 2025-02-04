
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


test_that("read gdb feature class and convert to sf", {
  fc_path <- file.path(system.file("extdata", "testing_data.gdb",
                                   package = "fluvgeo"),
                       "feature_dataset", "flowline")
  # rename to `gdb_fc2sf`
  sf <- fc2sf(fc_path)
  expect_true(class(sf)[1] == "sf")
})

test_that("read gdb table and convert to data frame using sf", {
  table_path <- file.path(system.file("extdata", "testing_data.gdb",
                                      package = "fluvgeo"),
                          "riffle_floodplain_dims_L1_table")
  # add `gdb_table2df`
})

test_that("read gdb raster and convert to terra::SpatRast", {
  raster_path <- file.path(system.file("extdata", "testing_raster.gdb",
                                       package = "fluvgeo"),
                           "dem_1m")

  raster <- gdb_raster2SpatRast(raster_path)
  expect_true("SpatRaster" %in% class(raster))
})

test_that("write sf to gdb feature class", {
  # add `sf2gdb_fc`
})

test_that("write data frame to gdb table", {
  # add `df2gdb_table``
})

test_that("Write terra::SpatRast to gdb raster", {
  # add `SpatRast2gdb_raster``
})
