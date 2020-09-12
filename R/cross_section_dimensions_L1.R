#' @title Calculate Cross Section Dimensions at Level 1
#'
#' @description  Calculates level 1 cross section dimensions for the input
#' cross section feature class.
#'
#' @export
#' @param xs                  SpatialLinesDataFrame; the full path to a
#'                            cross section line feature class
#' @param lead_n              numeric; The number of features to lead/lag on
#'                            either side of each feature that will be used to
#'                            calculate the slope and sinuosity.
#' @param use_smoothing       boolean; determines if smoothed elevation values
#'                            are used to calculate gradient. values are:
#'                            TRUE, FALSE (default)
#' @param loess_span          numeric; the loess regression span parameter,
#'                            defaults to 0.05
#' @param vert_units	        character; The vertical units. One of: "m"
#'                            (meter), "ft" (foot), "us-ft" (us survey foot)
#'
#' @return A new cross section data frame with the hydraulic geometry
#'      dimensions added.
#'
#'
#' @importFrom dplyr bind_rows select
#'
cross_section_dimensions_L1 <- function(xs, lead_n,
                                     use_smoothing, loess_span, vert_units) {
  # Check inputs
  check_cross_section(xs, "station_points")

  # Subset xs for the current reach
  xs_reach <- xs[xs@data$ReachName == unique(xs@data$ReachName), ]

  # Calculate slope and sinuosity for xs_reach
  xs_reach_ss <- fluvgeo::slope_sinuosity(xs_reach,
                                          lead_n = lead_n, lag_n = 0,
                                          use_smoothing = use_smoothing,
                                          loess_span = loess_span,
                                          vert_units = vert_units)
  return(xs_reach_ss)
}
