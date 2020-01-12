#' @title Calculate cross section hydraulic geometry metrics
#'
#' @description Calculates the hydraulic geometry metrics bankfull (width,
#'     depth, area, elevation), floodprone (width, depth, area, elevation),
#'     width-depth ratio, entrenchment ratio, and water surface elevation for
#'     the input cross section.
#'
#' @export
#' @param xs_points           data frame; a data frame of cross section points
#' @param stream              character; The name of the stream.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#'
#' @return A data frame of hydraulic dimensions for the specified cross section
#'    at the specified detrended bankfull elevation.
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
#'        \item{fp_area}{numeric; The cross sectional area at the flood prone
#'                       elevation, units: square feet.}
#'        \item{fp_width}{numeric; The cross section width at the flood prone
#'                        elevation, units: feet.}
#'        \item{fp_depth}{numeric; The maximum depth at the flood prone
#'                        elevation, units: feet.}
#'        \item{xs_width_depth_ratio}{numeric; The bankfull width to bankfull
#'                        depth ratio.}
#'        \item{xs_entrenchment_ratio}{numeric; The entrenchment ratio (flood
#'                        prone width) / (bankfull width).}
#'        \item{watersurface_elev}{numeric; The water surface elevation,
#'                        units: NAVD88 feet.}
#'        \item{bankfull_elev}{numeric; The bankfull elevation, units: NAVD88
#'                        feet.}
#'        \item{floodprone_elev}{numeric; The flood prone elevation, units:
#'                        NAVD88 feet.}
#'    }
#'
#' @details Add methods description for calculating flood prone, width-depth
#'     ratio, and entrenchment ratio. Calls \code{xs_geometry()}.
#'
#' @seealso
#' The \code{xs_metrics} function calls the \code{\link{xs_geometry}}
#' function. The \code{xs_metrics} function is called by the
#' \code{\link{xs_regional_metrics}} function, which is called by the
#' \code{\link{xs_dimensions}} function.
#'
#' @examples
#' # Extract attribute data from the fluvgeo::sin_riffle_floodplain_points_sp
#' # SpatialPointsDataFrame
#' sin_xs_points_df <- fluvgeo::sin_riffle_floodplain_points_sp@data
#'
#' # Call the xs_metrics function
#' sin_4 <- xs_metrics(xs_points = sin_xs_points_df,
#'                     stream = "Sinsinawa",
#'                     xs_number = 4,
#'                     bankfull_elevation = 103.5)
#'
#' @importFrom stats na.omit
#' @importFrom assertthat assert_that
#'
xs_metrics <- function(xs_points, stream, xs_number,
                             bankfull_elevation) {
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

  # Subset xs_points for the current cross section
  xs <- na.omit(xs_points[xs_points$ReachName == stream &
                          xs_points$Seq == xs_number, ])
  # Determine drainage area
  drainage_area <- unique(xs$Watershed_Area_SqMile)

  # Calculate cross section geometry at bankfull
  xs_dims <- xs_geometry(xs, bankfull_elevation)

  # Calculate cross section geometry at flood-prone
  fp_elevation <- bankfull_elevation + (bankfull_elevation - 100)
  fp_dims <- xs_geometry(xs, fp_elevation)

  # Calculate width-depth ratio
  # Use mean_depth instead of max_depth (xs_depth)
  # mean_depth = xs_area / xs_width
  xs_width_depth <- xs_dims$xs_width / (xs_dims$xs_area / xs_dims$xs_width)

  # Calculate entrenchment ratio
  xs_entrench <- fp_dims$xs_width / xs_dims$xs_width

  # Calculate Water surface elevation
  watersurface_elev <- xs[xs$Detrend_DEM_Z == min(xs$Detrend_DEM_Z), ]$DEM_Z

  # Define type of cross section
  xs_type <- c("DEM derived cross section")

  # Build data frame of results
  dims <- data.frame(stream, xs_number, xs_type, bankfull_elevation,
                     drainage_area, xs_dims$xs_area, xs_dims$xs_width,
                     xs_dims$xs_depth, xs_dims$discharge,
                     fp_dims$xs_area, fp_dims$xs_width,
                     fp_dims$xs_depth, xs_width_depth, xs_entrench,
                     watersurface_elev, xs_dims$ground_elev, fp_dims$ground_elev,
                     stringsAsFactors = FALSE)
  colnames(dims) <- c("reach_name", "cross_section", "xs_type",
                      "bankfull_elevation", "drainage_area",
                      "xs_area", "xs_width", "xs_depth", "discharge",
                      "fp_area", "fp_width", "fp_depth",
                      "xs_width_depth_ratio", "xs_entrenchment_ratio",
                      "watersurface_elev", "bankfull_elev",
                      "floodprone_elev")
  return(dims)
}
