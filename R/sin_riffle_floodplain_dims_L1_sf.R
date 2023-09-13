#' @docType data
#'
#' @title Sinsinawa Creek, IL stream riffle cross section lines
#'
#' @description  A \code{sf} object containing level 1 cross section dimensions
#' for Sinsinawa Creek, IL. Cross section dimension polyline feature classes are
#' used to store the hydraulic dimensions calculated for each cross section.
#' @format A \code{sf} with 10 observations and 32 variables.
#'
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Seq}{numeric; The cross section unique identifier.}
#'     \item{OBJECTID_1}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Watershed_Area_SqMile}{numeric; The upstream watershed drainage
#'                                  area.}
#'     \item{from_measure}{numeric; The start longitudinal stationing value
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
#'     \item{Z_smooth}{numeric; The smoothed z-coordinate value (elevation).}
#'     \item{upstream_x}{numeric; The x-coordinate value (longitude) of the next
#'                       upstream point in the moving window.}
#'     \item{upstream_y}{numeric; The y-coordinate value (latitude) of the next
#'                      upstream point in the moving window.}
#'     \item{downstream_x}{numeric; The x-coordinate value (longitude) of the
#'                        next downsteam point in the moving window.}
#'     \item{downstream_y}{numeric; The y-coordinate value (latitude) of the
#'                        next downsteam point in the moving window.}
#'     \item{upstream_z}{numeric; The z-coordinate value (elevation) of the
#'                      next upstream point in the moving window.}
#'     \item{downstream_z}{numeric; The z-coordinate value (elevation) of
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
"sin_riffle_floodplain_dims_L1_sf"
