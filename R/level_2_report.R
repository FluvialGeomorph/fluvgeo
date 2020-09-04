#' @title  Level 2 Report
#'
#' @description  Creates a level 2 report.
#'
#' @export
#' @param stream             character; The stream name. The stream name must
#'                           match a stream name in `ReachName` field in the
#'                           other parameters.
#' @param flowline_fc        character; The path to a `flowline` feature class.
#' @param xs_fc              character; The path to the cross section feature
#'                           class.
#' @param xs_points_fc       character; The path to a `xs_points` feature
#'                           class for the first time period.
#' @param xs_dims_fc         character; The path to the "xs_dims" feature class.
#' @param dem                character; The path to the DEM raster.
#' @param banklines_fc       character: the path to the banklines feature class.
#' @param features_fc        character; The path to a `features` feature class.
#' @param bf_estimate        numeric; Detrended bankfull estimate (units:
#'                           detrended feet).
#' @param regions            character vector; Regions to calculate hydraulic
#'                           dimensions for. See the `RegionalCurve` package for
#'                           a list of regions.
#' @param extent_factor      numeric; The extent factor used to control the
#'                           extent of cross section site maps.
#' @param label_xs           logical; Label cross sections?
#' @param show_xs_map        logical; Add the cross section maps to the report?
#' @param profile_units      character; the units of the longitudinal profile.
#'                           One of "kilometers", "meters", "miles", or "feet"
#' @param output_dir         character; The path to the folder in which to
#'                           write the report.
#' @param output_format      character; The file format of the report. One of
#'                           "html_document", "word_document", "pdf_document".
#'
#' @return Produces a FluvialGeomorph Level 2 Report in the `output_dir` in the
#' requested file format.
#'
#' @importFrom rmarkdown render
#'
level_2_report <- function(stream, flowline_fc, xs_fc, xs_points_fc,
                           xs_dims_fc, dem, banklines_fc, features_fc,
                           bf_estimate, regions, extent_factor, label_xs,
                           show_xs_map = FALSE,
                           profile_units, output_dir, output_format) {

  # Convert feature classes to sf objects
  flowline_sf  <- fluvgeo::fc2sf(flowline_fc)
  xs_sf        <- fluvgeo::fc2sf(xs_fc)
  xs_points_sf <- fluvgeo::fc2sf(xs_points_fc)
  xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
  banklines_sf <- fluvgeo::fc2sf(banklines_fc)
  features_sf  <- fluvgeo::fc2sf(features_fc)

  # Set report parameters
  report_params <- list("stream" = stream,
                        "flowline_sf" = flowline_sf,
                        "xs_sf" = xs_sf,
                        "xs_points_sf" = xs_points_sf,
                        "xs_dims_sf" = xs_dims_sf,
                        "dem" = dem,
                        "banklines_sf" = banklines_sf,
                        "features_sf" = features_sf,
                        "bf_estimate" = bf_estimate,
                        "regions" = regions,
                        "extent_factor" = extent_factor,
                        "label_xs" = label_xs,
                        "show_xs_map" = show_xs_map,
                        "profile_units" = profile_units,
                        "output_format" = output_format)

  # Define the report to use
  report_template <- system.file("reports", "level_2_report.Rmd",
                                 package = "fluvgeo")

  # Construct output_file path
  stream_name <- gsub(" ", "_", stream)
  if (output_format == "html_document") {extension <- ".html"}
  if (output_format == "word_document") {extension <- ".docx"}
  if (output_format == "pdf_document")  {extension <- ".pdf"}
  output_file <- file.path(output_dir, paste0(stream_name,
                                              "_level_2_report", extension))

  # Render the report
  rmarkdown::render(input = report_template,
                    output_format = output_format,
                    output_options = list(self_contained = TRUE),
                    params = report_params,
                    output_file = output_file)
}
