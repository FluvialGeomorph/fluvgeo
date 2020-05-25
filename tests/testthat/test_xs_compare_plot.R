library(purrr)
library(arcgisbinding)
arc.check_product()
library(fluvgeo)

stream <- "Cole Creek R1"
xs_number <- 40
xs_points_1 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2004_R1.gdb\\xs_50_points"
xs_points_2 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2010_R1.gdb\\xs_50_points"
xs_points_3 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\xs_50_points"
xs_points_4 <- ""
survey_name_1 <- "2004"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
survey_name_4 <- ""

# Create list of survey paths
print("Assembling survey events")
xs_points_paths <- list(xs_points_1, xs_points_2, xs_points_3, xs_points_4)

# Convert backslash paths to forward slash paths
xs_points_paths <- map(xs_points_paths, forward_slash)
xs_points_paths <- dplyr::na_if(xs_points_paths, "")

# Name the survey paths list by their survey names
survey_names <- c(survey_name_1, survey_name_2, survey_name_3, survey_name_4)
survey_names <- dplyr::na_if(survey_names, "")
xs_points_paths <- setNames(xs_points_paths, survey_names)
print(xs_points_paths)

# Eliminate empty surveys
print("Discarding empty items")
xs_points_paths <- purrr::discard(xs_points_paths, is.na)

# Convert list of survey paths to list of sp objects
print("Converting to sp")
xs_pts_sp <- purrr::map(xs_points_paths, fluvgeo::arc2sp)

# Call the graph function
print("Calling plot")
print(fluvgeo::xs_compare_plot(stream = stream,
                               xs_number = xs_number,
                               xs_pts_list = xs_pts_sp))
