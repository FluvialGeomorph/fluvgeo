cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/xs_50")
channel_fc     <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/channel_103")
floodplain_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                        package = "fluvgeo"),
                            "feature_dataset/floodplain_112")
dem_path <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                     "dem_2016_hydro_50")

cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
channel       <- fluvgeo::fc2sf(channel_fc, quiet = TRUE)
floodplain    <- fluvgeo::fc2sf(floodplain_fc, quiet = TRUE)
dem           <- gdb_raster2SpatRast(dem_path)
xs_number <- 110
extent_factor <- 1.5

test_that("check map_xs with sf inputs", {
  xs_map_sf <- fluvgeo::map_xs(cross_section = cross_section,
                               xs_number = xs_number,
                               dem = dem,
                               channel = channel,
                               floodplain = floodplain,
                               extent_factor = extent_factor)
  #print(xs_map_sf)


  expect_true("tmap" %in% class(xs_map_sf))
  expect_error(print(xs_map_sf), NA)
})

test_that("check map_xs with no channel and floodplain", {
  xs_map_nb <- map_xs(cross_section = cross_section,
                      xs_number = xs_number,
                      dem = dem,
                      extent_factor = extent_factor)
  #print(xs_map_nb)

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
                         channel = channel,
                         floodplain = floodplain,
                         extent_factor = extent_factor)
  #print(xs_map_sf_il)

  expect_true("tmap" %in% class(xs_map_sf_il))
  expect_error(print(xs_map_sf_il), NA)
})

