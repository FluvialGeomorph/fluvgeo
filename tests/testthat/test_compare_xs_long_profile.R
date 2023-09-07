library(purrr)
library(fluvgeo)

stream <- "Cole Creek R1"
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
survey_name_1 <- "2006"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
survey_name_4 <- NULL
features_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/features")
profile_units <- "feet"
label_xs = TRUE
xs_label_freq = 10

# Create list of survey paths
xs_points_paths <- list(xs_points_1, xs_points_2, xs_points_3, xs_points_4)

# Name the survey paths list by their survey names
survey_names <- c(survey_name_1, survey_name_2, survey_name_3, survey_name_4)
xs_points_paths <- setNames(xs_points_paths, survey_names)

# Eliminate empty surveys
xs_points_paths <- purrr::discard(xs_points_paths, is.null)

# Convert list of survey paths to list of sf objects
xs_pts_sf_list <- purrr::map(xs_points_paths, fluvgeo::fc2sf)

# Convert features to sp
features_sf <- fluvgeo::fc2sf(features_fc)

# Call the graph function
p <- compare_xs_long_profile(stream = stream,
                             xs_pts_sf_list = xs_pts_sf_list,
                             features_sf = features_sf,
                             profile_units = profile_units,
                             label_xs = label_xs,
                             xs_label_freq = xs_label_freq)
print(p)

test_that("compare_XS_long_profile exists", {
  expect_true("ggplot" %in% class(p))
  expect_error(print(p), NA)
})
