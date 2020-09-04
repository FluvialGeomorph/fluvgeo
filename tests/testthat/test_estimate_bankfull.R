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
stream <- "Sinsinawa"

xs_dims_fc    <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "riffle_channel_dims")
xs_points_fc  <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "riffle_channel_points")
features_fc   <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "features")

regions <- c("Eastern United States", "Illinois River")
bankfull_elevations <- seq(103, 104, 0.1)
bf_estimate <- 103.5
stat <- "MAE"
label_xs <- TRUE
profile_units <- "miles"
output_dir <- Sys.getenv("HOME")


test_that("The output html report exists", {
  skip_if_no_arc()
  load_libraries()

  output_format <- "html_document"
  output_file <- file.path(output_dir, paste0("bankfull_estimate_",
                                              "Sinsinawa_103_5.html"))

  # Call the estimate_bankfull function with test data
  estimate_bankfull(stream = stream,
                    xs_dims_fc = xs_dims_fc,
                    xs_points_fc = xs_points_fc,
                    features_fc = features_fc,
                    regions = regions,
                    bankfull_elevations = bankfull_elevations,
                    bf_estimate = bf_estimate,
                    stat = stat,
                    label_xs = label_xs,
                    profile_units = profile_units,
                    output_dir = output_dir,
                    output_format = output_format)
  expect_true(file.exists(output_file))
})

# Only test on systems with a working Tex distribution
# test_that("The output pdf report exists", {
#   estimate_bankfull(xs_points = sin_xs_points_df,
#                     streams = streams,
#                     regions = regions,
#                     bankfull_elevations = bankfull_elevations,
#                     bf_estimate = bf_estimate,
#                     stat = stat,
#                     output_dir = output_dir,
#                     output_format = "pdf_document")
#
#   output_file <- file.path(output_dir, paste0("bankfull_estimate_",
#                                               "Sinsinawa.pdf"))
#   expect_true(file.exists(output_file))
#   #file.remove(output_file)
# })

# Only test on systems with MS Word installed
# test_that("The output word report exists", {
#   estimate_bankfull(xs_points = sin_xs_points_df,
#                     streams = streams,
#                     regions = regions,
#                     bankfull_elevations = bankfull_elevations,
#                     bf_estimate = bf_estimate,
#                     stat = stat,
#                     output_dir = output_dir,
#                     output_format = "word_document")
#
#   output_file <- file.path(output_dir, paste0("bankfull_estimate_",
#                                               "Sinsinawa.docx"))
#   expect_true(file.exists(output_file))
#   #file.remove(output_file)
# })
