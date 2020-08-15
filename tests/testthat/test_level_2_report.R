library(fluvgeo)
context("level_2_report")

stream <- "Sinsinawa"
flowline_fc   <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                           "flowline")
xs_fc         <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                           "riffle_floodplain")
xs_points_fc  <- file.path(system.file("extdata", "testing_data.gdb",
                                          package = "fluvgeo"),
                           "riffle_floodplain_points")
xs_dims_fc    <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "riffle_floodplain_dims")
dem           <- file.path(system.file("extdata", "testing_raster.gdb",
                                       package = "fluvgeo"),
                           "dem_1m")
banklines_fc  <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "banklines")
features_fc   <- file.path(system.file("extdata", "testing_data.gdb",
                                       package = "fluvgeo"),
                           "features")
bf_estimate   <- 103.5
regions       <- c("Illinois River", "IL River LTE 300",
                   "Eastern United States")
extent_factor <- 1
label_xs      <- TRUE
output_dir    <- path.expand('~')                                   # tempdir()
output_format <- "word_document"

fluvgeo::level_2_report(stream, flowline_fc, xs_fc, xs_points_fc,
                        xs_dims_fc, dem, banklines_fc, features_fc,
                        bf_estimate, regions, extent_factor, label_xs,
                        output_dir, output_format)
