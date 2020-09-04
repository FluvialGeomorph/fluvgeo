#' @title Estimate bankfull report
#'
#' @description Produces the Estimate Bankfull report.
#'
#' @export
#' @param stream              character; The stream name. The stream name must
#'                            match a stream name in `ReachName` field in the
#'                            other parameters.
#' @param xs_dims_fc          character; The path to the "xs_dims" feature
#'                            class. This is for the "base year" survey.
#' @param xs_points_1         character; The path to a `xs_points` feature
#'                            class for the "base year".
#' @param xs_points_2         character; The path to a `xs_points` feature
#'                            class for the second time period.
#' @param xs_points_3         character; The path to a `xs_points` feature
#'                            class for the third time period.
#' @param xs_points_4         character; The path to a `xs_points` feature
#'                            class for the fourth time period.
#' @param survey_name_1       character: The name or date of the "base year"
#'                            survey.
#' @param survey_name_2       character: The name or date of the second survey.
#' @param survey_name_3       character: The name or date of the third survey.
#' @param survey_name_4       character: The name or date of the fourth survey.
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
estimate_bankfull <- function(stream, xs_dims_fc,
                              xs_points_1, xs_points_2,
                              xs_points_3, xs_points_4,
                              survey_name_1, survey_name_2,
                              survey_name_3, survey_name_4,
                              features_fc,
                              regions, bankfull_elevations, bf_estimate,
                              stat, label_xs, profile_units,
                              output_dir, output_format) {

  # Create list of survey paths
  xs_points_paths <- list(xs_points_1, xs_points_2, xs_points_3, xs_points_4)

  # Name the survey paths list by their survey names
  survey_names <- c(survey_name_1, survey_name_2, survey_name_3, survey_name_4)
  xs_points_paths <- setNames(xs_points_paths, survey_names)

  # Eliminate empty surveys
  xs_points_paths <- purrr::discard(xs_points_paths, is.null)

  # Convert list of survey paths to list of sf objects
  xs_pts_sf_list <- purrr::map(xs_points_paths, fluvgeo::fc2sf)

  # Convert feature classes to sf objects
  xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
  features_sf  <- fluvgeo::fc2sf(features_fc)

  # Set report parameters
  report_params <- list("stream" = stream,
                        "xs_dims_sf" = xs_dims_sf,
                        "xs_pts_sf_list" = xs_pts_sf_list,
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
  stream_name <- gsub(" ", "_", stream, fixed = TRUE)
  bf_est <- gsub(".", "_", bf_estimate, fixed = TRUE)
  output_file <- file.path(output_dir, paste0("bankfull_estimate_", stream_name,
                                              "_", bf_est, extension))

  # Render the report
  rmarkdown::render(input = report_template,
                    output_format = output_format,
                    output_options = list(self_contained = TRUE),
                    params = report_params,
                    output_file = output_file,
                    quiet = FALSE)
}
