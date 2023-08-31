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
#' xs_sp <- fluvgeo::sin_riffle_floodplain_dims_L3_sp
#'
#' # Call the xs2pts function for a cross section
#' xs_pts <- xs2pts(xs_sf)
#'
#' @importFrom sp SpatialPointsDataFrame
#'
xs2pts <- function(xs_sf) {
  coords      <- xs_sf[, c("POINT_X", "POINT_Y")]
  data        <- xs_sf
  crs <- sf::st_crs(xs_sf)

  coords      <- xs_sp@data[, c("POINT_X", "POINT_Y")]
  data        <- xs_sp@data
  proj4string <- xs_sp@proj4string

  xs_pts <- sp::SpatialPointsDataFrame(coords = coords,
                                       data = data,
                                       proj4string = proj4string)
  xs_sf_sp<-st_as_sf(xs_pts)

  xs_pts2<- xs_sf %>%
    select("POINT_X","POINT_Y")

  xs_pts3<-sf::st_cast(xs_pts2, to="POINT", group_or_split=TRUE)

  xs_pts2 <- sf::st_cast(xs_sf[,c("POINT_X", "POINT_Y")], to="POINT")
}
