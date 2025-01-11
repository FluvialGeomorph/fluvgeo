cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset//xs_50_dims_L1")
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
xs_number <- 8
extent_factor <- 2
xs_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL

# Create list of survey paths
xs_points_paths <- list(xs_points_1, xs_points_2, xs_points_3, xs_points_4)

# Name the survey paths list by their survey names
survey_names <- c(survey_name_1, survey_name_2, survey_name_3, survey_name_4)
xs_points_paths <- setNames(xs_points_paths, survey_names)

# Eliminate empty surveys
xs_points_paths <- purrr::discard(xs_points_paths, is.null)

# Convert list of survey paths to list of sf objects
xs_pts_sf_list <- purrr::map(xs_points_paths, fluvgeo::fc2sf)


test_that("fig_xs_profiles_L1 returns a patchwork object", {
  for(i in cross_section$Seq) {
    p1 <- fluvgeo::fig_xs_profiles_L1(cross_section = cross_section,
                                      xs_number = i,
                                      dem = dem,
                                      channel = channel,
                                      floodplain = floodplain,
                                      extent_factor = extent_factor,
                                      xs_pts_sf_list = xs_pts_sf_list)
    #print(p1)
    expect_true("patchwork" %in% class(p1))
  }
})
