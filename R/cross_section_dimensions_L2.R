#' @title Calculate Cross Section Dimensions at Level 2
#'
#' @description  Calculates level 2 cross section dimensions for the input
#' cross section feature class.
#'
#' @export
#' @param xs                  sf; the full path to a
#'                            cross section line feature class
#' @param xs_points           sf data frame; the full path to a
#'                            cross section points feature class
#' @param bankfull_elevation  numeric; The bankfull elevation (in feet) that is
#'                            used to calculate hydraulic geometry.
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
#' @importFrom dplyr bind_rows select
#'
cross_section_dimensions_L2 <- function(xs, xs_points, bankfull_elevation,
                                        lead_n,
                                        use_smoothing,
                                        loess_span = 0.5,
                                        vert_units) {
  # Check inputs
  check_cross_section(xs, "station_points")
  check_cross_section_points(xs_points, "station_points")

  # Create a list to hold the xs dimensions
  xs_geoms_ss <- list()
  xs_geoms    <- list()

  # Iterate through xs ReachNames
  for (g in unique(xs_points$ReachName)) {
    print(unique(xs_points$ReachName))

    # Subset xs for the current reach
    xs_reach <- xs[xs$ReachName == g, ]

    # Calculate slope and sinuosity for xs_reach
    xs_reach_ss <- fluvgeo::slope_sinuosity(xs_reach,
                                            lead_n = lead_n, lag_n = 0,
                                            use_smoothing = use_smoothing,
                                            loess_span = loess_span,
                                            vert_units = vert_units)
    xs_geoms_ss[[g]] <- xs_reach_ss
    message("slope and sinuosity complete")

    # Iterate through xs's and calculate dimensions
    for (i in xs[xs$ReachName == g, ]$Seq) {
      # Subset for the current stream and convert to data frame
      xs_pts <- xs_points[xs_points$ReachName == g, ]
      # Calculate xs dimensions
      dims <- fluvgeo::xs_metrics(xs_points = xs_pts,
                              stream = g,
                              xs_number = i,
                              bankfull_elevation = bankfull_elevation)
      xs_geoms[[i]] <- dims
      message(paste("xs", i, "metrics complete"))
    }
  }
  # Append the list of xs dimensions into a single data frame
  # (slope_sinuosity)
  reach_geoms <- dplyr::bind_rows(xs_geoms_ss)

  # Append the list of xs_points dimensions into a singe data frame
  # (xs_dimensions)
  xs_reach_geoms <- dplyr::bind_rows(xs_geoms)

  # Remove fields from reach_geoms calculated by xs_reach_geoms
  reach_geoms_2 <- reach_geoms[!(names(reach_geoms) %in% names(xs_reach_geoms))]

  # Join reach_geoms and xs_reach_geoms
  dims_join <- merge(x = reach_geoms_2,
                     y = xs_reach_geoms,
                     by.x = "Seq", by.y = "cross_section")
  message("join of reach_geoms and xs_reach_geoms complete")

  # Remove unneeded fields
  unneeded_names <- c("reach_name", "xs_type")
  dims_join_reduced <- dplyr::select(dims_join, -unneeded_names)
}
