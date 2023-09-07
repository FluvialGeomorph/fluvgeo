#' @title Calculate Cross Section Dimensions at Level 1
#'
#' @description  Calculates level 1 cross section dimensions for the input
#' cross section feature class.
#'
#' @export
#' @param xs_sf               sf object; the full path to a
#'                            cross section line feature class
#' @param lead_n              numeric; The number of features to lead/lag on
#'                            either side of each feature that will be used to
#'                            calculate the slope and sinuosity.
#' @param use_smoothing       boolean; determines if smoothed elevation values
#'                            are used to calculate gradient. values are:
#'                            TRUE, FALSE (default)
#' @param loess_span          numeric; the loess regression span parameter,
#'                            defaults to 0.05
#' @param vert_units	        character; The DEM vertical units. One of: "m"
#'                            (meter), "ft" (foot), "us-ft" (us survey foot)
#'
#' @return A new cross section data frame with the Level 1 hydraulic geometry
#' dimensions added.
#'
#'
cross_section_dimensions_L1 <- function(xs_sf, lead_n,
                                     use_smoothing, loess_span = 0.5,
                                     vert_units) {
  # Check inputs
  check_cross_section(xs_sf, "station_points")

  # Subset xs for the current reach
  xs_reach <- xs_sf[xs_sf$ReachName == unique(xs_sf$ReachName), ]

  # Calculate slope and sinuosity for xs_reach
  xs_reach_ss <- fluvgeo::slope_sinuosity(channel_features = xs_reach,
                                          lead_n = lead_n, lag_n = lead_n,
                                          use_smoothing = use_smoothing,
                                          loess_span = loess_span,
                                          vert_units = vert_units)
  return(xs_reach_ss)
}
