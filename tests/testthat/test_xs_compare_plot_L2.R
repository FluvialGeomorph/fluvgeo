library(purrr)
library(fluvgeo)

stream <- "Cole Creek R1"
xs_number <- 1
xs_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/riffle_floodplain_points")
xs_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/riffle_floodplain_points")
xs_points_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/riffle_floodplain_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL
bankfull_elevation <- 103
aspect_ratio = 0.4

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
p3 <- fluvgeo::xs_compare_plot_L2(stream = stream,
                                  xs_number = xs_number,
                                  xs_pts_sf_list = xs_pts_sf_list,
                                  bankfull_elevation = bankfull_elevation,
                                  aspect_ratio = aspect_ratio)
print(p3)

# Only one survey
stream <- "Cole Creek R1"
xs_number <- 1
xs_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "feature_dataset/riffle_floodplain_points")
xs_points_2 <- NULL
xs_points_3 <- NULL
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- NULL
survey_name_3 <- NULL
survey_name_4 <- NULL
bankfull_elevation <- 103
aspect_ratio = 0.5

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
p1 <- fluvgeo::xs_compare_plot_L2(stream = stream,
                                  xs_number = xs_number,
                                  xs_pts_sf_list = xs_pts_sf_list,
                                  bankfull_elevation = bankfull_elevation,
                                  aspect_ratio = aspect_ratio)
print(p1)


test_that("xs_compare plot 3 surveys", {
  expect_true("ggplot" %in% class(p3))
  expect_error(print(p3), NA)
})

test_that("xs_compare plot 1 survey", {
  expect_true("ggplot" %in% class(p1))
  expect_error(print(p1), NA)
})
