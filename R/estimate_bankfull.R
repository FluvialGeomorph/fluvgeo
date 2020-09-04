#' @title Estimate bankfull report
#'
#' @description Produces the Estimate Bankfull report.
#'
#' @export
#' @param stream              character; The stream name. The stream name must
#'                            match a stream name in `ReachName` field in the
#'                            other parameters.
#' @param xs_dims_fc          character; The path to the "xs_dims" feature
#'                            class.
#' @param xs_points_fc        character; The path to a `xs_points` feature
#'                            class for the first time period.
#' @param features_fc         character; The path to a `features` feature class.
#' @param regions             character; The regions that a dimension will be
#'                            calculated for. See the regional_curves$region
#'                            field for a complete list.
#' @param bankfull_elevations numeric vector; The bankfull elevations (units:
#'                            detrended feet) that are used to calculate
#'                            hydraulic geometry.
#' @param bf_estimate         numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#' @param stat                character; The statistic to graph "RMSE", "MAE"
#'                            (the default).
#' @param label_xs            logical; Label cross sections?
#' @param profile_units       character; the units of the longitudinal profile.
#'                            One of "kilometers", "meters", "miles", or "feet"
#' @param output_dir          character; The output directory.
#' @param output_format       character; The output format of the report. One
#'                            of "html_document", "word_document",
#'                            "pdf_document".
#'
#' @return Produces a FluvialGeomorph Estimate Bankfull report in the
#' `output_dir` in the requested file format.
#'
#' @examples
#'
#'
estimate_bankfull <- function(stream, xs_dims_fc, xs_points_fc, features_fc,
                              regions, bankfull_elevations, bf_estimate,
                              stat, label_xs, profile_units,
                              output_dir, output_format) {

  # Convert feature classes to sf objects
  xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
  xs_points_sf <- fluvgeo::fc2sf(xs_points_fc)
  features_sf  <- fluvgeo::fc2sf(features_fc)

  # Set report parameters
  report_params <- list("stream" = stream,
                        "xs_dims_sf" = xs_dims_sf,
                        "xs_points_sf" = xs_points_sf,
                        "features_sf" = features_sf,
                        "regions" = regions,
                        "bankfull_elevations" = bankfull_elevations,
                        "bf_estimate" = bf_estimate,
                        "stat" = stat,
                        "label_xs" = label_xs,
                        "profile_units" = profile_units,
                        "output_format" = output_format)

  # Define the report to use
  report_template <- system.file("reports", "estimate_bankfull_report.Rmd",
                                 package = "fluvgeo")

  # Construct output_file path
  if (output_format == "html_document") {extension <- ".html"}
  if (output_format == "word_document") {extension <- ".docx"}
  if (output_format == "pdf_document")  {extension <- ".pdf"}
  bf_est <- gsub(".", "_", bf_estimate, fixed = TRUE)
  output_file <- file.path(output_dir, paste0("bankfull_estimate_", stream,
                                              "_", bf_est, extension))

  # Render the report
  rmarkdown::render(input = report_template,
                    output_format = output_format,
                    output_options = list(self_contained = TRUE),
                    params = report_params,
                    output_file = output_file,
                    quiet = FALSE)
}
