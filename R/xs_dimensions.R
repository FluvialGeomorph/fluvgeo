#' @title Calculate cross section and regional metrics for a set of streams,
#'     regions, and detrended bankfull elevations
#'
#' @description Calculates a data frame of hydraulic geometry dimensions
#'     (area, width, depth) for a set of streams, regions, and detrended bankfull elevationsby cross section, xs type, and bankfull elevation.
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
#'
#' @return A data frame of cross section hydraulic geometry dimensions (area,
#'    width, depth, discharge) by cross section, xs type, and bankfull
#'    elevation.
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
#'        \item{discharge}{numeric; The estimated discharge at the specified
#'                         drainage area.}
#'    }
#'
#' @seealso The \code{xs_dimensions} function calls the
#' \code{\link{xs_regional_metrics}} function, which calls the
#' \code{\link{xs_metrics}} function, which calls the
#' \code{\link{xs_geometry}} function.
#'
#' @examples
#' # Extract attribute data from the fluvgeo::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fluvgeo::sin_riffle_channel_points_sp@@data
#'
#' # Call the xs_dimensions function with test data
#' streams <- c("Sinsinawa")
#' regions <- c("Eastern United States", "IN Central Till Plain")
#' bankfull_elevations = seq(103, 104, 0.1)
#'
#' sin <- xs_dimensions(xs_points = sin_xs_points_df,
#'                      streams = streams,
#'                      regions = regions,
#'                      bankfull_elevations = bankfull_elevations)
#'
#' @importFrom dplyr bind_rows
#' @importFrom assertthat assert_that
#'
xs_dimensions <- function(xs_points, streams, regions, bankfull_elevations) {
  # Check parameters
  assert_that(is.data.frame(xs_points),
              msg = "'xs_points' must be a data frame")
  assert_that("Seq" %in% colnames(xs_points),
              msg = "Required field 'Seq' is missing from 'xs_points'")
  assert_that("POINT_X" %in% colnames(xs_points),
              msg = "Required field 'POINT_X' is missing from 'xs_points'")
  assert_that("POINT_Y" %in% colnames(xs_points),
              msg = "Required field 'POINT_Y' is missing from 'xs_points'")
  assert_that("POINT_M" %in% colnames(xs_points),
              msg = "Required field 'POINT_M' is missing from 'xs_points'")
  assert_that("Watershed_Area_SqMile" %in% colnames(xs_points),
              msg = "Required field 'Watershed_Area_SqMile' is missing from
              'xs_points'")
  assert_that("km_to_mouth" %in% colnames(xs_points),
              msg = "Required field 'km_to_mouth' is missing from
              'xs_points'")
  assert_that("DEM_Z" %in% colnames(xs_points),
              msg = "Required field 'DEM_Z' is missing from 'xs_points'")
  assert_that("Detrend_DEM_Z" %in% colnames(xs_points),
              msg = "Required field 'Detrend_DEM_Z' is missing from
              'xs_points'")
  assert_that("ReachName" %in% colnames(xs_points),
              msg = "Required field 'ReachName' is missing from 'xs_points'")
  assert_that(is.character(streams) && length(streams) > 0,
              msg = "streams must be a character vector with at least one
                     item")
  assert_that(is.character(regions) && length(regions) > 0,
              msg = "regions must be a character vector with at least one
                     item")
  assert_that(is.numeric(bankfull_elevations) &&
              length(bankfull_elevations) > 0,
              msg = "bankfull_elevations must be a numeric vector with at
                     least one item")

  # Create a list to hold the cross section geometries
  xs_geoms <- list()
  f <- 1
  # Iterate through stream reaches
  for (g in streams) {
    # Iterate through regions
    for (h in regions) {
      # Iterate through bankfull elevations
      for (i in bankfull_elevations) {
        # Iterate through cross sectiions
        for (j in as.integer(levels(as.factor(
                        xs_points[xs_points$ReachName == g, ]$Seq)))) {
          # Calculate current cross section geometry
          xs_geoms[[f]] <- xs_regional_metrics(xs_points = xs_points,
                                               stream = g,
                                               xs_number = j,
                                               bankfull_elevation = i,
                                               region = h)
          f <- f + 1
          }
      }
    }
  }
  # Append all of the xs_geoms data frames
  xs_dims <- bind_rows(xs_geoms)
  return(xs_dims)
}
