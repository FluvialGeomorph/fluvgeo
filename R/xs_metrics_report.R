#' @title Cross Section Metrics Report
#'
#' @description Produces the cross section metrics report.
#'
#' @export
#' @param xs_points           data frame; a data frame of a cross section points
#'                            data struture
#' @param xs_dims_planform    SpatialLinesDataFrame; a
#'                            SpatialLinesDataFrame of a cross section planform
#'                            dimensions data structure
#' @param flowline            SpatialLinesDataFrame; a SpatialLinesDataFrame of
#'                            a flowline data structure
#' @param dem                 character; path to a dem raster
#' @param banklines           SpatialLinesDataFrame; a banklines feature class
#' @param extent_factor       numeric; A numeric value used to expand the map
#'                            extent around each cross section
#' @param streams             character vector; The stream names in the study
#'                            area.
#' @param regions             character; The regions that a dimension will be
#'                            calculated for. See the regional_curves$region
#'                            field for a complete list.
#' @param features            SpatialPointsDataFrame; a
#'                            SpatialPointsDataFrame of river features
#' @param label_xs            boolean; Draw the cross section labels?
#' @param output_dir          character; The output directory.
#' @param output_format       character; The output format of the report. One
#'                            of "html_document", "word_document",
#'                            "pdf_document".
#'
#' @return A report written to the file system in the output fromat requested.
#'
#' @examples
#' # Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fluvgeo::sin_riffle_floodplain_points_sp@@data
#'
#' # Get a cross section planform dimensions feature class
#' sin_xs_dims_planform <- fluvgeo::sin_riffle_floodplain_dims_planform_sp
#'
#' # Set variable values
#' dem <- system.file("extdata", "dem_1m.tif", package = "fluvgeo")
#' extent_factor <- 2
#' streams <- c("Sinsinawa")
#' regions <- c("Eastern United States", "IN Central Till Plain")
#' output_dir <- Sys.getenv("HOME")
#' output_format <- "word_document"
#'
#' # Create the xs Metrics Report
#' xs_metrics_report(xs_points = sin_xs_points_df,
#'                   xs_dims_planform = sin_xs_dims_planform,
#'                   flowline = fluvgeo::sin_flowline_sp,
#'                   dem = dem,
#'                   banklines = fluvgeo::sin_banklines_sp,
#'                   extent_factor = extent_factor,
#'                   streams = streams,
#'                   regions = regions,
#'                   features = fluvgeo::sin_features_sp,
#'                   label_xs = TRUE,
#'                   output_dir = output_dir,
#'                   output_format = output_format)
#'
xs_metrics_report <- function(xs_points, xs_dims_planform, flowline,
                              dem, banklines, extent_factor, streams, regions,
                              features, label_xs, output_dir, output_format) {
  # Iterate through streams
  for (g in streams) {
    # Determine `bf_estimate`
    bf_estimate <- unique(xs_dims_planform$bankfull_elevation)

    # Bankfull elevations to examine for sensitivity analysis
    bankfull_elevations <- bf_estimate

    # Construct output_file path
    if (output_format == "html_document") {extension <- ".html"}
    if (output_format == "word_document") {extension <- ".docx"}
    if (output_format == "pdf_document")  {extension <- ".pdf"}
    output_file <- file.path(output_dir, paste0("xs_dimensions_",
                                                g, "_", bf_estimate, extension))

    # Subset xs_points for the current stream
    xs_pts <- xs_points[xs_points$ReachName == g, ]
    xs_dims_plan <- xs_dims_planform[xs_dims_planform$ReachName == g, ]

    # Render the report for the current stream
    rmarkdown::render(input = system.file("reports", "xs_metrics_report.Rmd",
                                          package = "fluvgeo"),
                      output_format = output_format,
                      output_options = list(self_contained = TRUE),
                      params = list(xs_points = xs_pts,
                                    xs_dims_planform = xs_dims_plan,
                                    flowline = flowline,
                                    dem = dem,
                                    banklines = banklines,
                                    extent_factor = extent_factor,
                                    stream = g,
                                    regions = regions,
                                    bf_estimate = bf_estimate,
                                    features = features,
                                    label_xs = label_xs,
                                    output_format = output_format),
                      output_file = output_file,
                      quiet = FALSE)
  }
}
