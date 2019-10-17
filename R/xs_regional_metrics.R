#' @title Calculate cross section and regional hydraulic geometry metrics
#'
#' @description Calculates cross section geometry and regional hydraulic
#' geometry dimensions for the specified cross section at the specified
#' bankfull elevation.
#'
#' @export
#' @param xs_points           data frame; a data frame of cross section points
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
#'        \item{discharge}{numeric; The estimated discharge at the specified
#'                         drainage area.}
#'    }
#'
#' @seealso
#' The \code{xs_regional_metrics} function calls the \code{\link{xs_metrics}}
#' function which calls the \code{\link{xs_geometry}} function. The
#' \code{xs_regional_metrics} function is called by the
#' \code{\link{xs_dimensions}} function.
#'
#' @examples
#' # Extract attribute data from the fgm::sin_riffle_channel_points_sp
#' # SpatialPointsDataFrame
#' sin_xs_points_df <- fgm::sin_riffle_channel_points_sp@@data
#'
#' # Call the xs_metrics function
#' sin_4 <- xs_regional_metrics(xs_points = sin_xs_points_df,
#'                              stream = "Sinsinawa",
#'                              xs_number = 4,
#'                              bankfull_elevation = 103.5,
#'                              region = "Eastern United States")
#'
#' @importFrom RegionalCurve RHG
#' @importFrom dplyr bind_rows
#' @importFrom assertthat assert_that
#'
xs_regional_metrics <- function(xs_points, stream, xs_number,
                                bankfull_elevation, region) {
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
  assert_that(is.character(stream) && nchar(stream) != 0 &&
                length(stream) == 1,
              msg = "stream must be a character vector of length one")
  assert_that(xs_number%%1 == 0 && length(xs_number) == 1,
              msg = "xs_number must be an integer vector of length one")
  assert_that(is.numeric(bankfull_elevation) &&
              length(bankfull_elevation) == 1,
              msg = "bankfull_elevation must be a numeric vector of
              length one")
  assert_that(is.character(region) && length(region) == 1,
              msg = "region must be a character vector of length one")

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
  rhg_discharge <- RHG(region = region,
                       drainageArea = drainage_area,
                       dimensionType = "discharge")
  # Build a data frame of RHG results
  rhg <- data.frame(stream, xs_number, region, bankfull_elevation,
                    drainage_area, rhg_xs_area, rhg_width, rhg_depth,
                    rhg_discharge,
                    stringsAsFactors = FALSE)
  # Assign column names
  column_names <- c("reach_name", "cross_section", "xs_type",
                    "bankfull_elevation", "drainage_area", "xs_area",
                    "xs_width", "xs_depth", "discharge")
  # Assign column names to rhg data frame
  colnames(rhg) <- column_names
  # Build a data frame of xs geometry results
  xsg <- xs_geom[, column_names]
  # rbind rhg dimensions to xsg
  dims <- bind_rows(rhg, xsg)
  return(dims)
}
