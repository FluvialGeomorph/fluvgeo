#' @title Calculate cross section width, depth, area.
#'
#' @description Calculates the hydraulic geometry (width, depth, area, and
#'     ground elevation) for the input cross section at the specified
#'     detrended elevation.
#'
#' @export
#' @param xs_points         character; a data frame of cross section points.
#'                          Must be a single cross section.
#' @param detrend_elevation numeric; The detrended elevation (in feet) that
#'                          is used to calculate hydraulic geometry.
#'
#' @return A data frame of hydraulic dimensions at the specified detrended
#'    elevation. The data frame contains the fields:
#'    \describe{
#'        \item{xs_width}{numeric; The cross section width at the specified
#'                        detrended elevation.}
#'        \item{xs_depth}{numeric; The maximum depth at the specified
#'                        detrended elevation.}
#'        \item{xs_area}{numeric; The cross sectional area at the specified
#'                       detrended elevation.}
#'        \item{ground_elev}{numeric; The ground elevation of the detrended
#'                           elevation.}
#'    }
#'
#' @details The cross section points used as input to this function must represent
#'       only one cross section. See the documentation for the example
#'       \code{sin_xs_points} data frame for the specification of this input
#'       \code{sp::SpatialPointsDataFrame} object.
#'
#' @importFrom stats approxfun integrate
#'
#' @examples
#' # Extract the attribute data from the sin_xs_points SpatialPointsDataFrame
#' #     object
#' sin_xs_points_df <- sin_xs_points@@data
#'
#' # Subset the sin_xs_points data frame to contain only one cross section
#' #     (Seq = 4).
#' sin_xs_points_4 <- sin_xs_points_df[sin_xs_points_df$Seq == 4,]
#'
#' # Calculate hydraulic geometry for a single cross section
#' xs_geometry(xs_points = sin_xs_points_4, detrend_elevation = 103.5)
#'
xs_geometry <- function(xs_points, detrend_elevation) {
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
  f1 <- approxfun(x = xs_stations , y = ae - xs_dem_elev)
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
  # Construct output table
  xs_dims <- data.frame(xs_width, xs_depth, xs_area$value, ae)
  colnames(xs_dims) <- c("xs_width","xs_depth","xs_area","ground_elev")
  return(xs_dims)
}
