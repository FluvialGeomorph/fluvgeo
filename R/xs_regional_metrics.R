#' @title Calculate cross section and regional hydraulic geometry metrics
#'
#' @description Calculates cross section geometry and regional hydraulic
#' geometry dimensions for the specified cross section at the specified
#' bankfull elevation.
#'
#' @export
#' @param xs_points           \code{sp} object; The \code{sp} object
#'                            containing the \code{xs_points} for the study
#'                            area.
#' @param stream              character; The name of the stream.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation
#'                            (units: NAVD88 feet) that is used to calculate
#'                            hydraulic geometry.
#' @param region              character; The region that a dimension will be
#'                            calculated for. See the regional_curves$region
#'                            field for a complete list.
#'
#' @return A data frame of hydraulic dimensions for the specified cross
#'    section at the specified detrended bankfull elevation and the regional
#'    hydraulic dimensions.
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
#'                        detrended bankfull elevation, units: feet.}
#'        \item{xs_depth}{numeric; The maximum depth at the specified
#'                        detrended bankfull elevation, units: detrended
#'                        feet.}
#'    }
#'
#' @importFrom RegionalCurve RHG
#' @importFrom dplyr bind_rows
#'
xs_regional_metrics <- function(xs_points, stream, xs_number,
                                bankfull_elevation, region) {
  # Extract drainage area for the current xs from `XS_points` attribute table
  drainage_area <- unique(xs_points[xs_points$ReachName == stream &
                           xs_points$Seq == xs_number, ]$Watershed_Area_SqMile)
  # Calculate xs dimensions for current cross section
  xs_geom <- xs_metrics(xs_points = xs_points, stream = stream,
                        xs_number = xs_number,
                        bankfull_elevation = bankfull_elevation)
  # Calculate RHG channel dimensions for current cross section
  rhg_xs_area <- RHG(region = region,
                     drainageArea = drainage_area,
                     dimensionType = "area")
  rhg_width   <- RHG(region = region,
                     drainageArea = drainage_area,
                     dimensionType = "width")
  rhg_depth   <- RHG(region = region,
                     drainageArea = drainage_area,
                     dimensionType = "depth")
  # Build a data frame of RHG results
  rhg <- data.frame(stream, xs_number, region, bankfull_elevation,
                    drainage_area, rhg_xs_area, rhg_width, rhg_depth,
                    stringsAsFactors = FALSE)
  # Assign column names
  column_names <- c("reach_name", "cross_section", "xs_type",
                    "bankfull_elevation", "drainage_area", "xs_area",
                    "xs_width", "xs_depth")
  # Assign column names to rhg data frame
  colnames(rhg) <- column_names
  # Build a data frame of xs geometry results
  xsg <- xs_geom[, column_names]
  # rbind rhg dimensions to xsg
  dims <- bind_rows(rhg, xsg)
  return(dims)
}
