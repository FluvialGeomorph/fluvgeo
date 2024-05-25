cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/riffle_channel")
banklines_fc     <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeo"),
                              "feature_dataset/banklines")
dem_path        <- file.path(system.file("extdata",
                                         package = "fluvgeo"),
                             "dem_2016_hydro_50.tif")

cross_section <- fluvgeo::fc2sf(cross_section_fc, quiet = TRUE)
banklines     <- fluvgeo::fc2sf(banklines_fc, quiet = TRUE)
dem           <- terra::rast(dem_path)
xs_number <- 3
extent_factor <- 1.8
xs_points_1 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_3 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/xs_50_points")
xs_points_4 <- NULL
survey_name_1 <- "2004"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
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

# Call the graph function
p1 <- fig_xs_profiles(cross_section = cross_section,
                      xs_number = xs_number,
                      dem = dem,
                      banklines = banklines,
                      extent_factor = extent_factor,
                      xs_pts_sf_list = xs_pts_sf_list)
print(p1)

test_that("fig_xs_profiles a patchwork object", {
  expect_true("patchwork" %in% class(p1))
  expect_error(print(p1), NA)
})
