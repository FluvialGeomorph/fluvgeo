#' @title Calculate cross section width, depth, area.
#'
#' @description Calculates the hydraulic geometry (width, depth, area, and
#'     ground elevation) for the input cross section at the specified
#'     detrended elevation.
#'
#' @export
#' @param xs_points         data frame; a data frame of cross section points.
#'                          Must be a single cross section.
#' @param detrend_elevation numeric; The detrended elevation used to
#'                          calculate hydraulic geometry, units: detrended
#'                          feet.
#'
#' @return A data frame of hydraulic dimensions at the specified detrended
#'    elevation. The data frame contains the fields:
#'    \describe{
#'        \item{xs_width}{numeric; The cross section width at the specified
#'                        detrended elevation, units: feet.}
#'        \item{xs_depth}{numeric; The maximum depth at the specified
#'                        detrended elevation, units: feet.}
#'        \item{xs_area}{numeric; The cross sectional area at the specified
#'                       detrended elevation, units: square feet.}
#'        \item{ground_elev}{numeric; The ground elevation of the detrended
#'                           elevation, units: NAVD88 feet.}
#'    }
#'
#' @details The cross section points used as input to this function must
#'    represent only one cross section. See the documentation for the example
#'    \code{sin_xs_points} data frame for the specification of this input
#'    \code{sp::SpatialPointsDataFrame} object.
#'
#' @seealso
#' The \code{xs_geometry} function is called by the \code{\link{xs_metrics}}
#' function, which is called by the \code{\link{xs_regional_metrics}}
#' function, which is called by the \code{\link{xs_dimensions}} function.
#'
#' @examples
#' # Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fgm::sin_riffle_channel_points_sp@@data
#'
#' # Subset sin_xs_points to contain only one cross section (Seq = 4)
#' sin_xs_points_4 <- sin_xs_points_df[sin_xs_points_df$Seq == 4,]
#'
#' # Calculate hydraulic geometry for a single cross section
#' xs_geometry(xs_points = sin_xs_points_4, detrend_elevation = 103.5)
#'
#' @importFrom stats approxfun integrate
#' @importFrom assertthat assert_that
#'
xs_geometry <- function(xs_points, detrend_elevation) {
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
  assert_that(is.numeric(detrend_elevation))

  # Create local variables
  xs_stations        <- xs_points$POINT_M * 3.28084   # Convert meters to ft
  xs_dem_elev        <- xs_points$DEM_Z               # Elevations in feet

  # Convert detrended elevation to actual elevation
  eg <- mean(xs_points$DEM_Z - xs_points$Detrend_DEM_Z)
  ae <- detrend_elevation + eg

  ## Calculate the cross sectional area under a proposed bankfull elevation
  # Approach: the area between two curves is equal to the integral of the
  # difference between the two curves.
  # Calculate the difference between the two curves. This results in a new
  # curve. Then approximate a function for this "difference" curve.
  f1 <- approxfun(x = xs_stations, y = ae - xs_dem_elev)
  # Remove values above the bankfull elevation (negative values)
  f2 <- function(x) ifelse(f1(x) < 0, 0, f1(x))
  # Calculate the cross sectional area by taking the integral of the area
  # under the "difference" curve.
  xs_area <- integrate(f = f2, lower = min(xs_stations),
                       upper = max(xs_stations),
                       subdivisions = 100000,
                       stop.on.error = FALSE)

  # Calculate cross sectional width (cross section spacing * # of xs
  # depths > 0)
  d1 <- f2(xs_stations)
  xs_width <- mean(diff(xs_stations)) * length(d1[d1 > 0])

  # Calculate cross sectional max depth
  xs_depth <- max(f1(xs_stations)[f1(xs_stations) > 0])

  # Set discharge (as a placeholder field for other functions)
  discharge <- NA

  # Construct output table
  xs_dims <- data.frame(xs_width, xs_depth, xs_area$value, discharge, ae)
  colnames(xs_dims) <- c("xs_width", "xs_depth", "xs_area", "discharge",
                         "ground_elev")
  return(xs_dims)
}
