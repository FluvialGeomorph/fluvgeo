stream <- "Cole Creek R1"
flowline_fc      <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeodata"),
                              "feature_dataset/flowline")
cross_section_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                          package = "fluvgeodata"),
                              "feature_dataset/xs_50_dims_L1")
flowline_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                           package = "fluvgeodata"),
                               "feature_dataset/flowline_points")
flowline_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                           package = "fluvgeodata"),
                               "feature_dataset/flowline_points")
flowline_points_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                           package = "fluvgeodata"),
                               "feature_dataset/flowline_points")
flowline_points_4 <- NULL
xs_points_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/xs_50_points")
xs_points_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/xs_50_points")
xs_points_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/xs_50_points")
xs_points_4 <- NULL
survey_name_1 <- "2004"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
survey_name_4 <- NULL
features_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/features")
dem         <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "dem_2016_hydro_50")
show_xs_map <- TRUE
profile_units <- "feet"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 10
exaggeration = 30
extent_factor = 1.3
output_dir <- Sys.getenv("HOME")
output_format <- "word_document"


test_that("report completed - local path", {
  expected_report <- file.path(output_dir, "Cole_Creek_R1_level_1_report.docx")
  if (file.exists(expected_report)) file.remove(expected_report)

  fluvgeo::level_1_report(stream, flowline_fc, cross_section_fc,
                        flowline_points_1, flowline_points_2,
                        flowline_points_3, flowline_points_4,
                        xs_points_1, xs_points_2, xs_points_3, xs_points_4,
                        survey_name_1, survey_name_2,
                        survey_name_3, survey_name_4,
                        features_fc, dem, show_xs_map, profile_units,
                        aerial, elevation,
                        xs_label_freq, exaggeration, extent_factor,
                        output_dir, output_format)

  expect_true(file.exists(expected_report))
})

test_that("report completed - network path", {
  network_dir <- "//mvd/mvr/EGIS/Work/FluvialGeomorph"
  output_dir <- file.path(network_dir, "temp")
  dir.create(output_dir, showWarnings = FALSE)
  expected_report <- file.path(output_dir, "Cole_Creek_R1_level_1_report.docx")
  if (file.exists(expected_report)) file.remove(expected_report)

  fluvgeo::level_1_report(stream, flowline_fc, cross_section_fc,
                        flowline_points_1, flowline_points_2,
                        flowline_points_3, flowline_points_4,
                        xs_points_1, xs_points_2, xs_points_3, xs_points_4,
                        survey_name_1, survey_name_2,
                        survey_name_3, survey_name_4,
                        features_fc, dem, show_xs_map, profile_units,
                        aerial, elevation,
                        xs_label_freq, exaggeration, extent_factor,
                        output_dir, output_format)

  expect_true(file.exists(expected_report))
})
