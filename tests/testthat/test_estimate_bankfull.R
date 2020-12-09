library(fluvgeo)
context("estimate_bankfull")

# Helper functions
skip_if_no_arc <- function() {
  testthat::skip_if_not_installed("arcgisbinding")
}

load_libraries <- function() {
  library(sp)
  library(sf)
  library(tmap)
  library(arcgisbinding)
  arcgisbinding::arc.check_product()
}

# sf
stream <- "Cole Creek R1"
flowline_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "flowline")
xs_dims_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                       package = "fluvgeo"),
                         "riffle_channel_dims_L2")
xs_points_ch_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                            "riffle_channel_points")
xs_points_ch_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                     package = "fluvgeo"),
                            "riffle_channel_points")
xs_points_ch_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                     package = "fluvgeo"),
                            "riffle_channel_points")
xs_points_ch_4 <- NULL
xs_points_fp_1 <- file.path(system.file("extdata", "y2016_R1.gdb",
                                        package = "fluvgeo"),
                            "riffle_floodplain_points")
xs_points_fp_2 <- file.path(system.file("extdata", "y2010_R1.gdb",
                                        package = "fluvgeo"),
                            "riffle_floodplain_points")
xs_points_fp_3 <- file.path(system.file("extdata", "y2006_R1.gdb",
                                        package = "fluvgeo"),
                            "riffle_floodplain_points")
xs_points_fp_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2006"
survey_name_4 <- NULL
features_fc <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "features")
dem         <- file.path(system.file("extdata", "y2016_R1.gdb",
                                     package = "fluvgeo"),
                         "dem_2016_hydro_50")
show_xs_map <- FALSE
regions <- c("Eastern United States", "USA")
bankfull_elevations <- seq(104, 106, 0.2)
bf_estimate <- 105
stat <- "MAE"
label_xs <- TRUE
profile_units <- "miles"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 2
exaggeration = 30
extent_factor = 1.3
output_dir <- Sys.getenv("HOME")

test_that("The output docx report exists", {
  skip_if_no_arc()
  load_libraries()

  output_format <- "word_document"
  output_file <- file.path(output_dir, paste0("Cole_Creek_R1_105_",
                                              "bankfull_estimate.docx"))

  # Call the estimate_bankfull function with test data
  estimate_bankfull(stream = stream,
                    flowline_fc = flowline_fc,
                    xs_dims_fc = xs_dims_fc,
                    xs_points_ch_1 = xs_points_ch_1,
                    xs_points_ch_2 = xs_points_ch_2,
                    xs_points_ch_3 = xs_points_ch_3,
                    xs_points_ch_4 = xs_points_ch_4,
                    xs_points_fp_1 = xs_points_fp_1,
                    xs_points_fp_2 = xs_points_fp_2,
                    xs_points_fp_3 = xs_points_fp_3,
                    xs_points_fp_4 = xs_points_fp_4,
                    survey_name_1 = survey_name_1,
                    survey_name_2 = survey_name_2,
                    survey_name_3 = survey_name_3,
                    survey_name_4 = survey_name_4,
                    features_fc = features_fc,
                    dem = dem,
                    show_xs_map = show_xs_map,
                    regions = regions,
                    bankfull_elevations = bankfull_elevations,
                    bf_estimate = bf_estimate,
                    stat = stat,
                    label_xs = label_xs,
                    profile_units = profile_units,
                    aerial = aerial,
                    elevation = elevation,
                    xs_label_freq = xs_label_freq,
                    exaggeration = exaggeration,
                    extent_factor = extent_factor,
                    output_dir = output_dir,
                    output_format = output_format)
  expect_true(file.exists(output_file))
})

