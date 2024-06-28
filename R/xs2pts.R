#' @title XS lines to points
#'
#' @description Converts a `fluvgeo` cross section data structure from lines
#' to points.
#'
#' @export
#' @param xs_sf          sf data frame; a cross section data structure
#'                       created by the fluvgeo package.
#'
#' @return Returns a sf::sf data frame of cross section lines converted
#' to points.
#'
#' @examples
#' # Retrieve a cross section data structure
#' xs_sf <- fluvgeo::sin_riffle_floodplain_dims_L3_sf
#'
#' # Call the xs2pts function for a cross section
#' xs_pts <- xs2pts(xs_sf)
#'
#' @importFrom sf st_drop_geometry st_as_sf st_geometry st_sf
#'
xs2pts <- function(xs_sf) {
  sf_df<-st_drop_geometry(xs_sf)

  xs_pts <- sf_df %>%
    sf::st_as_sf(coords=c('POINT_X','POINT_Y')) %>%
    sf::st_geometry()%>%
    sf::st_sf(sf_df, crs=st_crs(xs_sf))
}
