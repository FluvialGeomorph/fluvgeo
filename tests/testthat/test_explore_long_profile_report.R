stream <- "Cole Creek R1"
fl_pts_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                  package = "fluvgeo"),
                      "feature_dataset/flowline_points")
fl_pts_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                  package = "fluvgeo"),
                      "feature_dataset/flowline_points")
fl_pts_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                  package = "fluvgeo"),
                      "feature_dataset/flowline_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL
dem           <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "dem_2016_hydro_50")
features_fc   <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                          "feature_dataset/features")
label_xs      <- TRUE
show_xs_map   <- TRUE
profile_units <- "miles"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 2
exaggeration = 20
extent_factor = 1.3
output_dir    <- Sys.getenv("HOME")
output_format <- "html_document"

fl_pts_1_sf <- fc2sf(fl_pts_1)
fl_pts_2_sf <- fc2sf(fl_pts_2)
fl_pts_3_sf <- fc2sf(fl_pts_3)
features_sf <- fc2sf(features_fc)
