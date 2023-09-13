#' @docType data
#'
#' @title Sinsinawa Creek, IL stream flowline
#'
#' @description  A \code{sf} object containing the streams center flowline. This
#'is the primary flow path feature used in the FluvialGeomorph toolbox. Since
#'non-bathymetric LiDAR is often used to derive flowline features, this should
#'not be interpreted as the thalweg when water is present in the DEM.
#'
#' @format A \code{sf} with 1 observations and 5
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{ReachName}{character; The name of the reach. }
#'      \item{from_measure}{numeric; The start longitudinal stationing value for
#'                       the reach in km.}
#'     \item{to_measure}{numeric; The end longitudinal stationing value for the
#'                        reach in km.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_flowline_sf"
