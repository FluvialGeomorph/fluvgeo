#' @title XS lines to points
#'
#' @description Converts a `fluvgeo` cross section data structure from lines
#' to points.
#'
#' @export
#' @param xs_sp          SpatialLinesDataFrame; a cross section data structure
#'                       created by the fluvgeo package.
#'
#' @return Returns a sp::SpatialPointsDataFrame of cross section lines converted
#' to points.
#'
#' @examples
#' # Retrieve a cross section data structure
#' xs_sp <- fluvgeo::sin_riffle_floodplain_dims_planform_sp
#'
#' # Call the xs2pts function for a cross section
#' xs_pts <- xs2pts(xs_sp)
#'
#' @importFrom sp SpatialPointsDataFrame
#'
xs2pts <- function(xs_sp) {
  coords      <- xs_sp@data[, c("POINT_X", "POINT_Y")]
  data        <- xs_sp@data
  proj4string <- xs_sp@proj4string

  xs_pts <- sp::SpatialPointsDataFrame(coords = coords,
                                       data = data,
                                       proj4string = proj4string)
}
