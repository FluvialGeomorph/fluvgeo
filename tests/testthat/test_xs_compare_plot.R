library(purrr)
library(fluvgeo)

stream <- "Cole Creek R1"
xs_number <- 40
xs_points_1 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2004_R1.gdb\\xs_50_points"
xs_points_2 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2010_R1.gdb\\xs_50_points"
xs_points_3 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\xs_50_points"
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
print(fluvgeo::xs_compare_plot(stream = stream,
                               xs_number = xs_number,
                               xs_pts_sf_list = xs_pts_sf_list))
