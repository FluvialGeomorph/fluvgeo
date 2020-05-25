library(dplyr)
library(tibble)
library(purrr)
library(arcgisbinding)
arc.check_product()
library(fluvgeo)

stream <- "Cole Creek R1"
flowline_points_1 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2004_R1.gdb\\flowline_points"
flowline_points_2 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2010_R1.gdb\\flowline_points"
flowline_points_3 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\flowline_points"
flowline_points_4 <- NULL
survey_name_1 <- "2004"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
survey_name_4 <- NULL
features_fc <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\features"
profile_units <- "feet"

# Create list of survey paths
print("Assembling survey events")
flowline_points_paths <- list(flowline_points_1, flowline_points_2,
                              flowline_points_3, flowline_points_4)

# Name the survey paths list by their survey names
survey_names <- c(survey_name_1, survey_name_2, survey_name_3, survey_name_4)
flowline_points_paths <- setNames(flowline_points_paths, survey_names)

# Eliminate empty surveys
print("Discarding empty items")
flowline_points_paths <- purrr::discard(flowline_points_paths, is.null)

# Convert list of survey paths to list of sp objects
print("Converting to sp")
flowline_pts_sp_list <- purrr::map(flowline_points_paths, fluvgeo::arc2sp)

# Convert features_fc to an sp object
features_sp <- fluvgeo::arc2sp(features_fc)

# Call the graph function
print("Calling plot")
print(fluvgeo::compare_long_profile(stream = stream,
                                    flowline_pts_sp_list = flowline_pts_sp_list,
                                    features_sp = features_sp,
                                    profile_units = profile_units))
