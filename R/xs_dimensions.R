#' @title Calculate cross section and regional metrics for a set of streams,
#'     regions, and detrended bankfull elevations
#'
#' @description Calculates a data frame of hydraulic geometry dimensions
#'     (area, width, depth) for a set of streams, regions, and detrended bankfull elevationsby cross section, xs type, and bankfull elevation.
#'
#' @export
#' @param xs_points           \code{sp} object; The \code{sp} object
#'                            containing the \code{xs_points} for the study
#'                            area.
#' @param streams             character vector; The stream names in the study
#'                            area.
#' @param regions             character; The regions that a dimension will be
#'                            calculated for. See the regional_curves$region
#'                            field for a complete list.
#' @param bankfull_elevations numeric vector; The bankfull elevations (units:
#'                            detrended feet) that are used to calculate
#'                            hydraulic geometry.
#'
#' @return A data frame of cross section hydraulic geometry dimensions (area,
#'    width, depth) by cross section, xs type, and bankfull elevation.
#'    \describe{
#'        \item{reach_name}{character; The name of the stream.}
#'        \item{cross_section}{numeric; The cross section unique identifier.
#'                         Seq is only unique within a reach.}
#'        \item{xs_type}{character; A string indicating how the cross section
#'                       was derived. "DEM derived cross section" denotes
#'                       dimensions calculated from the DEM and "<Region
#'                       Name>" denotes that the dimensions were calculated
#'                       from that regions regional curve.}
#'        \item{bankfull_elevation}{numeric; The detrended bankfull elevation
#'                       (in feet) that is used to calculate hydraulic
#'                       geometry.}
#'        \item{drainage_area}{numeric; The area of the watershed upstream
#'                       from this cross section, units: square miles.}
#'        \item{xs_area}{numeric; The cross sectional area at the specified
#'                       detrended bankfull elevation, units: square feet.}
#'        \item{xs_width}{numeric; The cross section width at the specified
#'                       detrended bankfull elevation, units: feet.}
#'        \item{xs_depth}{numeric; The maximum depth at the specified
#'                       detrended bankfull elevation, units: detrended
#'                       feet.}
#'    }
#'
#' @importFrom dplyr bind_rows
#'
xs_dimensions <- function(xs_points, streams, regions, bankfull_elevations) {
  # Create a list to hold the cross section geometries
  xs_geoms <- list()
  f = 1
  # Iterate through stream reaches
  for (g in streams) {
    # Iterate through regions
    for (h in regions) {
      # Iterate through bankfull elevations
      for (i in bankfull_elevations) {
        # Iterate through cross sectiions
        for (j in as.integer(levels(as.factor(
                        xs_points[xs_points$ReachName == g,]$Seq)))) {
          # Calculate current cross section geometry
          xs_geoms[[f]] <- xs_regional_metrics(xs_points = xs_points,
                                               stream = g,
                                               xs_number = j,
                                               bankfull_elevation = i,
                                               region = h)
          f = f + 1
          }
      }
    }
  }
  # Append all of the xs_geoms data frames
  xs_dims <- bind_rows(xs_geoms)
  return(xs_dims)
}
