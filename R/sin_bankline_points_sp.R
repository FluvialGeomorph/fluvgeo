#' @docType data
#'
#' @title Sinsinawa Creek, IL stream bankline points
#'
#' @description  A \code{sp::SpatialPointsDataFrame} object containing stream
#' bankline points for Sinsinawa Creek, IL. These points represent the bankfull
#' bankline extracted from a LiDAR terrain surface.
#'
#' @format A \code{SpatialPointsDataFrame} with 1413 observations and 9
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{ReachName}{character; The name of the stream. }
#'     \item{bank_id}{numeric; The bank unique identifier. Right descending = 1,
#'                    left descending = 2. }
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_bankline_points_sp"
