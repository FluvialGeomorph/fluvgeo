cross_section_fc <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_channel")
banklines_fc     <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/banklines")
dem_path        <- file.path(system.file("extdata",
                                         package = "fluvgeo"),
                             "dem_1m.tif")

cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
banklines     <- fluvgeo::fc2sf(banklines_fc, quiet = TRUE)
dem           <- terra::rast(dem_path)
xs_number <- 3
extent_factor <- 1.5

test_that("check map_xs with sf inputs", {
  xs_map_sf <- fluvgeo::map_xs(cross_section = cross_section,
                               xs_number = xs_number,
                               dem = dem,
                               banklines = banklines,
                               extent_factor = extent_factor)
  print(xs_map_sf)

  expect_true("tmap" %in% class(xs_map_sf))
  expect_error(print(xs_map_sf), NA)
})

test_that("check map_xs with no banklines", {
  xs_map_nb <- map_xs(cross_section = cross_section,
                      xs_number = xs_number,
                      dem = dem,
                      extent_factor = extent_factor)
  print(xs_map_nb)

  expect_true("tmap" %in% class(xs_map_nb))
  expect_error(print(xs_map_nb), NA)
})


test_that("check map_xs with different coordinate system inputs", {
  # Reproject to IL Sf W USFT
  ilspwusft <- sf::st_crs(3436)
  cross_section_il <- sf::st_transform(cross_section, crs = ilspwusft)

  # Create map
  xs_map_sf_il <- map_xs(cross_section = cross_section_il,
                         xs_number = xs_number,
                         dem = dem,
                         banklines = banklines,
                         extent_factor = extent_factor)
  print(xs_map_sf_il)

  expect_true("tmap" %in% class(xs_map_sf_il))
  expect_error(print(xs_map_sf_il), NA)
})

