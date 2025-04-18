#' @docType data
#'
#' @title Sinsinawa Creek, IL stream riffle cross section lines
#'
#' @description  A \code{sf} object containing manually drawn cross sections for
#' Sinsinawa Creek, IL. Riffles are irregularly spaced along a stream and are
#' manually drawn by a GIS analyst. Analysts are trained to identify riffles
#' using a set of guidelines from the literature and interpretation of the DEM
#' and aerial imagery.
#'
#' @format A \code{sf} with 10 observations and 14 variables.
#'
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Watershed_Area_SqMile}{numeric; The upstream watershed drainage area.}
#'     \item{Seq}{numeric; The cross section unique identifier.}
#'     \item{from_measure}{numeric; 	The start longitudinal stationing value
#'                        for the reach in km.}
#'     \item{to_measure}{numeric; The end longitudinal stationing value for the
#'                        reach in km.}
#'     \item{ReachName}{character; The name of the reach.}
#'     \item{POINT_X}{numeric; The x-coordinate value (longitude).}
#'     \item{POINT_Y}{numeric; The y-coordinate value (latitude).}
#'     \item{POINT_M}{numeric; The m-coordinate value (route position).}
#'     \item{Z}{numeric;The z-coordinate value (elevation).}
#'     \item{km_to_mouth}{numeric; The distance to the mouth of the site in km.}
#'     \item{loop}{numeric; The loop number.}
#'     \item{bend}{numeric; The bend number.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_riffle_floodplain_sf"
