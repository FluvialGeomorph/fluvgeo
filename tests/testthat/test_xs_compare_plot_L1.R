stream <- "Cole Creek R1"
xs_number <- 8
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
p1 <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                  xs_number = xs_number,
                                  xs_pts_sf_list = xs_pts_sf_list)
print(p1)

p2 <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                  xs_number = xs_number,
                                  xs_pts_sf_list = xs_pts_sf_list,
                                  extent = "channel")
print(p2)

p3 <- fluvgeo::xs_compare_plot_L1(stream = stream,
                                  xs_number = xs_number,
                                  xs_pts_sf_list = xs_pts_sf_list,
                                  extent = "floodplain")
print(p3)

test_that("xs_compare_plot_L1 extent = all exists", {
  expect_true("ggplot" %in% class(p1))
  expect_error(print(p1), NA)
})

test_that("xs_compare_plot_L1 extent = channel exists", {
  expect_true("ggplot" %in% class(p2))
  expect_error(print(p2), NA)
})

test_that("xs_compare_plot_L1 extent = floodplain exists", {
  expect_true("ggplot" %in% class(p3))
  expect_error(print(p3), NA)
})
