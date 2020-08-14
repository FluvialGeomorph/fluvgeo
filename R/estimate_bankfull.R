#' @title Estimate bankfull report
#'
#' @description Produces the bankfull estimate report.
#'
#' @export
#' @param xs_points           data frame; a data frame of cross section points
#' @param streams             character vector; The stream names in the study
#'                            area.
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
#' @param output_dir          character; The output directory.
#' @param output_format       character; The output format of the report. One
#'                            of "html_document", "word_document",
#'                            "pdf_document".
#'
#' @return A report written to the file system in the output fromat requested.
#'
#' @examples
#' # Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fluvgeo::sin_xs_points@@data
#'
#' # Set variable values
#' streams <- c("Sinsinawa")
#' regions <- c("Eastern United States", "IN Central Till Plain")
#' bankfull_elevations <- seq(103, 104, 0.1)
#' bf_estimate <- 103.5
#' stat <- "MAE"
#' output_dir <- Sys.getenv("HOME")
#' output_format <- "html_document"
#'
#' # Call the estimate_bankfull function with test data
#' estimate_bankfull(xs_points = sin_xs_points_df,
#'                   streams = streams,
#'                   regions = regions,
#'                   bankfull_elevations = bankfull_elevations,
#'                   bf_estimate = bf_estimate,
#'                   stat = stat,
#'                   output_dir = output_dir,
#'                   output_format = output_format)
#'
estimate_bankfull <- function(xs_points, streams, regions, bankfull_elevations,
                              bf_estimate, stat,
                              output_dir, output_format) {
  # Iterate through streams
  for (g in streams) {
    # Construct output_file path
    if (output_format == "html_document") {extension <- ".html"}
    if (output_format == "word_document") {extension <- ".docx"}
    if (output_format == "pdf_document")  {extension <- ".pdf"}
    output_file <- file.path(output_dir, paste0("bankfull_estimate_",
                                                g, "_", bf_estimate, extension))

    # Subset xs_points for the current stream
    xs_pts <- xs_points[xs_points$ReachName == g, ]

    # Render the report for the current stream
    rmarkdown::render(input = system.file("reports",
                                          "estimate_bankfull_report.Rmd",
                                          package = "fluvgeo"),
                      output_format = output_format,
                      output_options = list(self_contained = TRUE),
                      params = list(xs_points = xs_pts,
                                    stream = g,
                                    regions = regions,
                                    bankfull_elevations = bankfull_elevations,
                                    bf_estimate = bf_estimate,
                                    stat = stat,
                                    output_format = output_format),
                      output_file = output_file,
                      quiet = FALSE)
  }
}
