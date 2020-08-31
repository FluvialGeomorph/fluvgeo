library(fluvgeo)
context("xs_metrics_report")

# Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fluvgeo::sin_riffle_floodplain_points_sp@data

# Get a cross section planform dimensions feature class
sin_xs_dims_planform <- fluvgeo::sin_riffle_floodplain_dims_planform_sp

# Set variable values
dem <- system.file("extdata", "dem_1m.tif", package = "fluvgeo")
extent_factor <- 2
streams <- c("Sinsinawa")
regions <- c("Eastern United States", "IN Central Till Plain")
output_dir <- Sys.getenv("HOME")
output_format <- "word_document"

# Create the xs Metrics Report
xs_metrics_report(xs_points = sin_xs_points_df,
                  xs_dims_planform = sin_xs_dims_planform,
                  flowline = fluvgeo::sin_flowline_sp,
                  dem = dem,
                  banklines = fluvgeo::sin_banklines_sp,
                  extent_factor = extent_factor,
                  streams = streams,
                  regions = regions,
                  features = fluvgeo::sin_features_sp,
                  label_xs = TRUE,
                  output_dir = output_dir,
                  output_format = output_format)

test_that("check report exists", {
  expect_true(file.exists(file.path(output_dir,
                                    "xs_dimensions_Sinsinawa_103.docx")))
})

