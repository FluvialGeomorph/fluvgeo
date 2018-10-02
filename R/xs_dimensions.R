#' @title Calculate cross section and regional dimensions
#'
#' @description Creates a data frame to hold hydraulic geometry dimensions (area, width,
#' depth) by cross section, xs type, and bankfull elevation.
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
#'
xs_Dimensions <- function(xs_points, streams, regions, bankfull_elevations) {
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
  xs_dims <- dplyr::bind_rows(xs_geoms)
  return(xs_dims)
}
