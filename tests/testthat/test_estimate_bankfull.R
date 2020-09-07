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
flowline_fc <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                     package = "fluvgeo"),
                         "flowline")
xs_dims_fc  <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                         "riffle_floodplain_dims")
xs_points_1 <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                     package = "fluvgeo"),
                         "riffle_floodplain_points")
xs_points_2 <- file.path(system.file("extdata", "testing_Cole_2010.gdb",
                                     package = "fluvgeo"),
                         "riffle_floodplain_points")
xs_points_3 <- file.path(system.file("extdata", "testing_Cole_2004.gdb",
                                     package = "fluvgeo"),
                         "riffle_floodplain_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2004"
survey_name_4 <- NULL
features_fc   <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "features")
regions <- c("Eastern United States", "USA")
bankfull_elevations <- seq(101, 104, 0.1)
bf_estimate <- 102.5
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
  output_file <- file.path(output_dir, paste0("Cole_Creek_R1_102_5_",
                                              "bankfull_estimate_.docx"))

  # Call the estimate_bankfull function with test data
  estimate_bankfull(stream = stream,
                    flowline_fc = flowline_fc,
                    xs_dims_fc = xs_dims_fc,
                    xs_points_1 = xs_points_1,
                    xs_points_2 = xs_points_2,
                    xs_points_3 = xs_points_3,
                    xs_points_4 = xs_points_4,
                    survey_name_1 = survey_name_1,
                    survey_name_2 = survey_name_2,
                    survey_name_3 = survey_name_3,
                    survey_name_4 = survey_name_4,
                    features_fc = features_fc,
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

