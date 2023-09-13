#' @docType data
#'
#' @title Sinsinawa Creek, IL stream riffle cross section points
#'
#' @description  A \code{sf} object cross section points for
#' Sinsinawa Creek, IL. These points are used to store elevation and route
#' position (lateral stream stationing) information for a cross section feature
#' class. Cross section points are used to calculate a series of cross section
#' dimensions.
#'
#' @format A \code{sf} with 3522 observations and 11 variables.
#'
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Seq}{numeric; The cross section unique identifier.}
#'     \item{POINT_X}{numeric; The x-coordinate value (longitude).}
#'     \item{POINT_Y}{numeric; The y-coordinate value (latitude).}
#'     \item{POINT_M}{numeric; The m-coordinate value (route position).}
#'     \item{Watershed_Area_SqMile}{numeric; The upstream watershed drainage area.}
#'     \item{ReachName}{character; The name of the reach.}
#'     \item{km_to_mouth}{numeric; The distance to the mouth of the site in km.}
#'     \item{DEM_Z}{numeric; The elevation of the point. By convention, the
#'                  vertical units used in this project are NADV88 feet.}
#'     \item{Detrend_DEM_Z}{numeric; The detrended elevation of the point.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_riffle_floodplain_points_sf"
