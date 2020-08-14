library(fluvgeo)
context("estimate_bankfull")

# Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fluvgeo::sin_riffle_floodplain_points_sp@data

# Set variable values
streams <- c("Sinsinawa")
regions <- c("Eastern United States", "IN Central Till Plain")
bankfull_elevations <- seq(103, 104, 0.1)
bf_estimate <- 103.5
stat <- "MAE"
output_dir <- Sys.getenv("HOME")
output_format <- "html_document"

# Call the estimate_bankfull function with test data
estimate_bankfull(xs_points = sin_xs_points_df,
                  streams = streams,
                  regions = regions,
                  bankfull_elevations = bankfull_elevations,
                  bf_estimate = bf_estimate,
                  stat = stat,
                  output_dir = output_dir,
                  output_format = output_format)

# Can be tested on all systems
test_that("The output html report exists", {
  output_file <- file.path(output_dir, paste0("bankfull_estimate_",
                                              "Sinsinawa_103.5.html"))
  expect_true(file.exists(output_file))
  #file.remove(output_file)
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
