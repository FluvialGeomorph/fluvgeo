library(fluvgeo)
context("level_2_report")

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
flowline_fc   <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                          package = "fluvgeo"),
                           "flowline")
xs_fc         <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                          package = "fluvgeo"),
                           "xs_250_25")
xs_dims_fc    <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "xs_250_25_dims")
xs_points_1 <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                     package = "fluvgeo"),
                         "xs_250_25_points")
xs_points_2 <- file.path(system.file("extdata", "testing_Cole_2010.gdb",
                                     package = "fluvgeo"),
                         "xs_250_25_points")
xs_points_3 <- file.path(system.file("extdata", "testing_Cole_2004.gdb",
                                     package = "fluvgeo"),
                         "xs_250_25_points")
xs_points_4 <- NULL
survey_name_1 <- "2016"
survey_name_2 <- "2010"
survey_name_3 <- "2004"
survey_name_4 <- NULL
dem           <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "dem_2016_ft")
banklines_fc  <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "banklines")
features_fc   <- file.path(system.file("extdata", "testing_Cole_2016.gdb",
                                       package = "fluvgeo"),
                           "features")
bf_estimate   <- 103.5
regions       <- c("USA", "Eastern United States")
label_xs      <- TRUE
show_xs_map   <- FALSE
profile_units <- "miles"
aerial <- TRUE
elevation = TRUE
xs_label_freq = 2
exaggeration = 30
extent_factor = 1.3
output_dir    <- Sys.getenv("HOME")
output_format <- "word_document"


test_that("check level 2 report, without xs map", {
  skip_if_no_arc()
  load_libraries()

  # Create report
  p <- fluvgeo::level_2_report(stream, flowline_fc, xs_fc, xs_dims_fc,
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
  print(p)
  expect_true(file.exists(file.path(output_dir,
                                    "Sinsinawa_level_2_report.docx")))
})

# test_that("check level 2 report, with xs map", {
#   skip_if_no_arc()
#   load_libraries()
#
#   # Create report
#   p <- fluvgeo::level_2_report(stream, flowline_fc, xs_fc, xs_dims_fc,
#                                xs_points_1, xs_points_2,
#                                xs_points_3, xs_points_4,
#                                survey_name_1, survey_name_2,
#                                survey_name_3, survey_name_4,
#                                dem, banklines_fc, features_fc,
#                                bf_estimate, regions, label_xs,
#                                show_xs_map, profile_units,
#                                aerial, elevation,
#                                xs_label_freq, exaggeration, extent_factor,
#                                output_dir, output_format)
#   print(p)
#   expect_true(file.exists(file.path(output_dir,
#                                     "Sinsinawa_level_2_report.docx")))
# })
