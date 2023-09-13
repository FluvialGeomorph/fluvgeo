#' @docType data
#'
#' @title Sinsinawa Creek, IL stream loop points
#'
#' @description  A \code{sf} object containing the streams loop points.The
#' loop_points sf object is used to store the longitudinal extents of loops and
#' bends used for planform analysis.
#'
#' @format A \code{sf} with 21 observations and 6
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'      \item{loop}{numeric; The loop number.}
#'     \item{bend}{numeric; The bend number.}
#'     \item{position}{character; The planform location indicator. Indicates if
#'     a bankline point represents the “start” of a loop bend, the “end” of a
#'      loop bend, or the “apex” of a loop.}
#'     \item{ReachName}{character; The name of the reach. }
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_loop_points_sf"
