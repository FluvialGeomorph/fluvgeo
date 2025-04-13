stream <- "Cole Creek R1"
flowline_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/flowline")
xs_dims_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeodata"),
                         "feature_dataset/riffle_channel_dims_L2")
xs_points_ch_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                            "feature_dataset/riffle_channel_points")
xs_points_ch_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeodata"),
                            "feature_dataset/riffle_channel_points")
xs_points_ch_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeodata"),
                            "feature_dataset/riffle_channel_points")
xs_points_ch_4 <- NULL
xs_points_fp_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                        package = "fluvgeodata"),
                            "feature_dataset/riffle_floodplain_points")
xs_points_fp_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                        package = "fluvgeodata"),
                            "feature_dataset/riffle_floodplain_points")
xs_points_fp_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                        package = "fluvgeodata"),
                            "feature_dataset/riffle_floodplain_points")
xs_points_fp_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL
features_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "feature_dataset/features")
dem         <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeodata"),
                         "dem_2016_hydro_50")
show_xs_map <- TRUE
regions <- c("Eastern United States", "USA")
bankfull_elevations <- seq(104, 106, 0.2)
bf_estimate <- 105
stat <- "MAE"
label_xs <- TRUE
profile_units <- "miles"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 10
exaggeration = 10
extent_factor = 1.2
output_dir <- Sys.getenv("HOME")
output_format <- "word_document"


test_that("The output docx report exists", {
  expected_report <- file.path(output_dir, paste0("Cole_Creek_R1_105_",
                                                  "bankfull_estimate.docx"))
  if (file.exists(expected_report)) file.remove(expected_report)

  estimate_bankfull(stream, flowline_fc, xs_dims_fc,
                    xs_points_ch_1, xs_points_ch_2,
                    xs_points_ch_3, xs_points_ch_4,
                    xs_points_fp_1, xs_points_fp_2,
                    xs_points_fp_3, xs_points_fp_4,
                    survey_name_1, survey_name_2,
                    survey_name_3, survey_name_4,
                    features_fc, dem, show_xs_map, regions,
                    bankfull_elevations, bf_estimate,
                    stat, label_xs, profile_units,
                    aerial, elevation,
                    xs_label_freq, exaggeration, extent_factor,
                    output_dir, output_format)

  expect_true(file.exists(expected_report))
})

test_that("The output docx report exists", {
  network_dir <- "//mvd/mvr/EGIS/Work/FluvialGeomorph"
  output_dir <- file.path(network_dir, "temp")
  dir.create(output_dir, showWarnings = FALSE)
  expected_report <- file.path(output_dir, paste0("Cole_Creek_R1_105_",
                                                  "bankfull_estimate.docx"))
  if (file.exists(expected_report)) file.remove(expected_report)

  estimate_bankfull(stream, flowline_fc, xs_dims_fc,
                    xs_points_ch_1, xs_points_ch_2,
                    xs_points_ch_3, xs_points_ch_4,
                    xs_points_fp_1, xs_points_fp_2,
                    xs_points_fp_3, xs_points_fp_4,
                    survey_name_1, survey_name_2,
                    survey_name_3, survey_name_4,
                    features_fc, dem, show_xs_map, regions,
                    bankfull_elevations, bf_estimate,
                    stat, label_xs, profile_units,
                    aerial, elevation,
                    xs_label_freq, exaggeration, extent_factor,
                    output_dir, output_format)

  expect_true(file.exists(expected_report))
})
