#' @title Calculate cross section dimensions
#'
#' @description  Calculates cross section hydraulic geometry dimensions for
#'     the input cross section feature class.
#'
#' @export
#' @param xs                  SpatialLinesDataFrame; the full path to a
#'                            cross section line feature class
#' @param xs_points           SpatialPointsDataFrame; the full path to a
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
#'
#' @return A new cross section feature class with the hydraulic geometry
#'      dimensions added to the attribute table
#'
#' @importFrom dplyr bind_rows append select
#'
cross_section_dimensions <- function(xs, xs_points, bankfull_elevation,
                                     lead_n, use_smoothing, loess_span) {
  # Check inputs
  check_cross_section(xs, "station_points")
  check_cross_section_points(xs_points, "station_points")

  # Create a list to hold the xs dimensions
  xs_geoms_ss <- list()
  xs_geoms    <- list()

  # Iterate through xs ReachNames
  for (g in unique(xs_points@data$ReachName)) {
    print(unique(xs_points@data$ReachName))

    # Subset xs for the current reach
    xs_reach <- xs[xs@data$ReachName == g, ]

    # Calculate slope and sinuosity for xs_reach
    xs_reach_ss <- fgm::slope_sinuosity(xs_reach,
                                        lead_n = lead_n, lag_n = 0,
                                        use_smoothing = use_smoothing,
                                        loess_span = loess_span)
    xs_geoms_ss[[g]] <- xs_reach_ss
    message("slope and sinuosity complete")

    # Iterate through xs's and calculate dimensions
    for (i in xs[xs@data$ReachName == g, ]$Seq) {
      # Subset for the current stream and convert to data frame
      xs_pts <- xs_points@data[xs_points@data$ReachName == g, ]
      # Calculate xs dimensions
      dims <- fgm::xs_metrics(xs_points = xs_pts,
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

  # Join reach_geoms and xs_reach_geoms
  dims_join <- merge(x = reach_geoms,
                     y = xs_reach_geoms,
                     by.x = "Seq", by.y = "cross_section")
  message("join of reach_geoms and xs_reach_geoms complete")

  # Remove fields from dims_join already on xs
  # Get the list of names from xs
  xs_names <- names(xs@data)
  # Retain the field `Seq` for the join
  xs_names <- xs_names[xs_names != "Seq"]
  # Add other fields to be removed
  xs_names <- append(xs_names, c("reach_name", "xs_type"))

  # Remove the uneeded fields
  dims_join_reduced <- select(dims_join, -xs_names)
}
