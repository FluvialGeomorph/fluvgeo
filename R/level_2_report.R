#' @title  Level 2 Report
#'
#' @description  Creates the FluvialGeomorph Level 2 report.
#'
#' @export
#' @param stream             character; The stream name. The stream name must
#'                           match a stream name in `ReachName` field in the
#'                           other parameters.
#' @param flowline_fc        character; The path to a `flowline` feature class.
#' @param xs_fc              character; The path to the cross section feature
#'                           class.
#' @param xs_dims_fc         character; The path to the "xs_dims" feature class.
#' @param xs_points_1        character; The path to a `xs_points` feature
#'                           class for the "base year".
#' @param xs_points_2        character; The path to a `xs_points` feature
#'                           class for the second time period.
#' @param xs_points_3        character; The path to a `xs_points` feature
#'                           class for the third time period.
#' @param xs_points_4        character; The path to a `xs_points` feature
#'                           class for the fourth time period.
#' @param survey_name_1      character: The name or date of the "base year"
#'                           survey.
#' @param survey_name_2      character: The name or date of the second survey.
#' @param survey_name_3      character: The name or date of the third survey.
#' @param survey_name_4      character: The name or date of the fourth survey.
#' @param dem                character; The path to the DEM raster.
#' @param banklines_fc       character: the path to the banklines feature class.
#' @param features_fc        character; The path to a `features` feature class.
#' @param bf_estimate        numeric; Detrended bankfull estimate (units:
#'                           detrended feet).
#' @param regions            character vector; Regions to calculate hydraulic
#'                           dimensions for. See the `RegionalCurve` package for
#'                           a list of regions.
#' @param label_xs           logical; Label cross sections?
#' @param show_xs_map        logical; Add the cross section maps to the report?
#' @param profile_units      character; the units of the longitudinal profile.
#'                           One of "kilometers", "meters", "miles", or "feet".
#' @param aerial             logical; Display an overview map with an aerial
#'                           photo background?
#' @param elevation          logical; Display an overview map with an elevation
#'                           background?
#' @param xs_label_freq      numeric; An integer indicating the frequency of
#'                           cross section labels.
#' @param exaggeration       numeric; The degree of terrain exaggeration.
#' @param extent_factor      numeric; The amount the extent is expanded around
#'                           the cross section feature class. Values greater
#'                           than one zoom out, values less than one zoom in.
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
level_2_report <- function(stream, flowline_fc, xs_fc, xs_dims_fc,
                           xs_points_1, xs_points_2,
                           xs_points_3, xs_points_4,
                           survey_name_1, survey_name_2,
                           survey_name_3, survey_name_4,
                           dem, banklines_fc, features_fc,
                           bf_estimate, regions, label_xs,
                           show_xs_map = FALSE, profile_units,
                           aerial = TRUE, elevation = FALSE,
                           xs_label_freq = 5, exaggeration = 10,
                           extent_factor = 1.2,
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
  flowline_sf  <- fluvgeo::fc2sf(flowline_fc)
  xs_sf        <- fluvgeo::fc2sf(xs_fc)
  xs_dims_sf   <- fluvgeo::fc2sf(xs_dims_fc)
  banklines_sf <- fluvgeo::fc2sf(banklines_fc)
  features_sf  <- fluvgeo::fc2sf(features_fc)

  # Set report parameters
  report_params <- list("stream" = stream,
                        "flowline_sf" = flowline_sf,
                        "xs_sf" = xs_sf,
                        "xs_dims_sf" = xs_dims_sf,
                        "xs_pts_sf_list" = xs_pts_sf_list,
                        "dem" = dem,
                        "banklines_sf" = banklines_sf,
                        "features_sf" = features_sf,
                        "bf_estimate" = bf_estimate,
                        "regions" = regions,
                        "label_xs" = label_xs,
                        "show_xs_map" = show_xs_map,
                        "profile_units" = profile_units,
                        "aerial" = aerial,
                        "elevation" = elevation,
                        "xs_label_freq" = xs_label_freq,
                        "exaggeration" = exaggeration,
                        "extent_factor" = extent_factor,
                        "output_format" = output_format)

  # Define the report to use
  report_template <- system.file("reports", "level_2_report.Rmd",
                                 package = "fluvgeo")

  # Construct output_file path
  if (output_format == "html_document") {extension <- ".html"}
  if (output_format == "word_document") {extension <- ".docx"}
  if (output_format == "pdf_document")  {extension <- ".pdf"}
  stream_name <- gsub(" ", "_", stream, fixed = TRUE)
  bf_est <- gsub(".", "_", bf_estimate, fixed = TRUE)
  output_file <- file.path(output_dir, paste0(stream_name, "_", bf_est,
                                              "_level_2_report", extension))

  # Render the report
  rmarkdown::render(input = report_template,
                    output_format = output_format,
                    output_options = list(self_contained = TRUE),
                    params = report_params,
                    output_file = output_file)
}
