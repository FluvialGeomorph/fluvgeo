#' @docType data
#'
#' @title Sinsinawa Creek, IL stream channel cross section points
#'
#' @description  A \code{sf} object containing stream
#' cross section points for two cross sections (Seq 4, 5). These points
#' represent elevation values extracted from a LiDAR terrain surface along a
#' transect perpendicular to the stream centerline.
#'
#' @format A \code{sf} with 1413 observations and 9
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Seq}{numeric; The cross section unique identifier. Seq is only
#'                unique within a reach.}
#'     \item{POINT_X}{numeric; The longitude or easting of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{st_crs(sin_xs_points)}.}
#'     \item{POINT_Y}{numeric; The latitude or northing of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{st_crs(sin_xs_points)}.}
#'     \item{POINT_M}{numeric; The route distance of this cross section from
#'                    the upstream end of the reach. Coordinate system units
#'                    can be determined by checking the
#'                    \code{st_crs(sin_xs_points)}.}
#'     \item{Watershed_Area_SqMile}{numeric; The area of the watershed upstream
#'                    from this cross section in square miles.}
#'     \item{km_to_mouth}{numeric; Distance downstream from this cross section
#'                    to the end of the reach.}
#'     \item{DEM_Z}{numeric; The elevation of the point. By convention, the
#'                  vertical units used in this project are NADV88 feet.}
#'     \item{Detrend_DEM_Z}{numeric; The detrended elevation of the point.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_riffle_channel_points_sf"
