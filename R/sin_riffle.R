#' @docType data
#'
#' @title Sinachwine Creek, IL riffles
#'
#' @description  A \code{sp::SpatialLinesDataFrame} object containing stream
#' riffles (Seq 1 - 10). These features represent the location of riffles
#' delineated as lines running perpendicular to the stream centerline.
#'
#' @format A \code{SpatialLinesDataFrame} with 10 observations and 11
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Watershed_Area_SqMile}{numeric; The area of the watershed upstream
#'                    from this cross section in square miles.}
#'     \item{Seq}{numeric; The cross section unique identifier. Seq is only
#'                unique within a reach.}
#'     \item{from_measure}{numeric; The route measurement start value. }
#'     \item{to_measure}{numeric; The route mesurement end value. }
#'     \item{ReachName}{character; The name of the stream. }
#'     \item{POINT_X}{numeric; The longitude or easting of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{sin_xs_points@@proj4string}.}
#'     \item{POINT_Y}{numeric; The latitude or northing of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{sin_xs_points@@proj4string}.}
#'     \item{POINT_M}{numeric; The route distance of this cross section from
#'                    the upstream end of the reach. Coordinate system units
#'                    can be determined by checking the
#'                    \code{sin_xs_points@@proj4string}.}
#'     \item{Z}{numeric; The elevation of the point. By convention, the
#'                  vertical units used in this project are NADV88 feet.}
#'     \item{km_to_mouth}{numeric; Distance downstream from this cross section
#'                    to the end of the reach.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_riffle"
