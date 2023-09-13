#' @docType data
#'
#' @title Sinsinawa Creek, IL stream valleyline
#'
#' @description  A \code{sf} object containing stream
#' gradient line for Sinsinawa Creek, IL. This polyline sf object represents
#' the trend line of the down-valley axis. It bisects stream meanders as the
#'  stream swings from side to side across its floodplain.
#'
#' @format A \code{sf} with 1 observations and 7
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{ReachName}{character; 	Name of the reach.}
#'     \item{from_measure}{numeric; .The start longitudinal stationing value for
#'                       the reach in km.}
#'     \item{to_measure}{numeric; The end longitudinal stationing value for the
#'                        reach in km.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_valleyline_sf"
