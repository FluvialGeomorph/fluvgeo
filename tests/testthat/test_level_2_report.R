library(fluvgeo)
context("level_2_report")

stream <- "Cole Creek R1"
flowline_fc   <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "flowline")
xs_fc         <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "xs_50")
xs_dims_fc    <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "xs_50_dims_L2")
xs_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "xs_50_points")
xs_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeo"),
                         "xs_50_points")
xs_points_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeo"),
                         "xs_50_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL
dem           <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "dem_2016_hydro_50")
banklines_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "banklines")
features_fc   <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                           "features")
bf_estimate   <- 103.5
regions       <- c("USA", "Eastern United States")
label_xs      <- TRUE
show_xs_map   <- FALSE
profile_units <- "miles"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 10
exaggeration = 30
extent_factor = 1.3
output_dir    <- Sys.getenv("HOME")
output_format <- "word_document"


test_that("check level 2 report, without xs map", {
  expected_report <- file.path(output_dir,
                               "Cole_Creek_R1_103_5_level_2_report.docx")
  if (file.exists(expected_report)) file.remove(expected_report)

  fluvgeo::level_2_report(stream, flowline_fc, xs_fc, xs_dims_fc,
                          xs_points_1, xs_points_2,
                          xs_points_3, xs_points_4,
                          survey_name_1, survey_name_2,
                          survey_name_3, survey_name_4,
                          dem, banklines_fc, features_fc,
                          bf_estimate, regions, label_xs,
                          show_xs_map, profile_units,
                          aerial, elevation,
                          xs_label_freq, exaggeration, extent_factor,
                          output_dir, output_format)

  expect_true(file.exists(expected_report))
})

test_that("check level 2 report, without xs map, network drive", {
  network_dir <- "//mvrdfs.mvr.ds.usace.army.mil/EGIS/Work/FluvialGeomorph"
  output_dir <- file.path(network_dir, "temp")
  dir.create(output_dir, showWarnings = FALSE)
  expected_report <- file.path(output_dir,
                               "Cole_Creek_R1_103_5_level_2_report.docx")
  if (file.exists(expected_report)) file.remove(expected_report)

  fluvgeo::level_2_report(stream, flowline_fc, xs_fc, xs_dims_fc,
                          xs_points_1, xs_points_2,
                          xs_points_3, xs_points_4,
                          survey_name_1, survey_name_2,
                          survey_name_3, survey_name_4,
                          dem, banklines_fc, features_fc,
                          bf_estimate, regions, label_xs,
                          show_xs_map, profile_units,
                          aerial, elevation,
                          xs_label_freq, exaggeration, extent_factor,
                          output_dir, output_format)

  expect_true(file.exists(expected_report))
})
