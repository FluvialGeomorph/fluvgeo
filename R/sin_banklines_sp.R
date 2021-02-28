#' @docType data
#'
#' @title Sinsinawa Creek, IL stream banklines
#'
#' @description  A \code{sp::SpatialLinesDataFrame} object containing stream
#' banklines for Sinsinawa Creek, IL. These lines represent the bankfull
#' banklines extracted from a LiDAR terrain surface.
#'
#' @format A \code{SpatialLinesDataFrame} with 2 observations and 7
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{ReachName}{character; The name of the stream. }
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_banklines_sp"
