#' @docType data
#'
#' @title Sinsinawa Creek, IL stream gradient points
#'
#' @description  A \code{sf} object containing stream
#' gradient points for Sinsinawa Creek, IL. These points represent elevation
#' values extracted from a LiDAR terrain surface along the flowline of the
#' stream.
#'
#' @format A \code{sf} with 3521 observations and 23
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{OBJECTID_1}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{ReachName}{character; The name of the reach.}
#'     \item{POINT_X}{numeric; The longitude or easting of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{st_crs(sin_gradient_sf)}.}
#'     \item{POINT_Y}{numeric; The latitude or northing of the point.
#'                    Coordinate system units can be determined by checking the
#'                    \code{st_crs(sin_gradient_sf)}.}
#'     \item{POINT_M}{numeric; The route distance of this cross section from
#'                    the upstream end of the reach. Coordinate system units
#'                    can be determined by checking the
#'                    \code{st_crs(sin_gradient_sf)}.}
#'     \item{Z}{numeric; The elevation of the point. By convention, the
#'                  vertical units used in this project are NADV88 feet.}
#'     \item{Z_smooth}{numeric; The smoothed elevation of the point. By
#'                      convention, the vertical unites used in this project are
#'                      NAVD88 feet.}
#'     \item{upstream_x}{numeric; The x-coordinate value (longitude) of the next
#'                       upstream point in the moving window.}
#'     \item{upstream_y}{numeric; The y-coordinate value (latitude) of the next
#'                      upstream point in the moving window.}
#'     \item{downstream_x}{numeric; The x-coordinate value (longitude) of the
#'                        next downsteam point in the moving window.}
#'     \item{downstream_y}{numeric; The y-coordinate value (latitude) of the
#'                        next downsteam point in the moving window.}
#'     \item{upstream_m}{numeric; The m-coordinate value (route position) of the
#'                      next upstream point in the moving window.}
#'     \item{downstream_m}{numeric; The m-coordinate value (route position) of
#'                        the next downstream point in the moving window.}
#'     \item{rise}{numeric; The elevation difference between the downstream and
#'                upstream point in the moving window, units are in feet.}
#'     \item{run}{numeric; The longitudinal distance along the stream network
#'                between the downstream and upstream point in the moving window,
#'                units are in feet.}
#'     \item{stream_length}{numeric; 	The longitudinal distance along the stream
#'                          network between the downstream and upstream point in
#'                          the moving window, units are in feet.}
#'     \item{valley_length}{numeric; The longitudinal distance along the
#'                          valleyline between the downstream and upstream
#'                          stream network point in the moving window, units are
#'                          in feet.}
#'    \item{sinuosity}{numeric; The sinuosity of the stream within the moving
#'                    window (valley_length/stream_length).}
#'    \item{sinuosity_gte_one}{numeric;The sinuosity metric, values greater than
#'                            or equal to one.}
#'    \item{slope}{numeric; The slope of the stream within the moving window
#'                (rise/run).}
#'    \item{slope_gte_zero}{numeric; The slope metric, values greater than of
#'                          equal to zero.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_gradient_sf"
