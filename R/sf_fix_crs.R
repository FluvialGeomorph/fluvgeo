#' @title Fix SF CRS
#'
#' @description Fixes mis-specified coordinate reference system (CRS) with 
#'              respect to the geospatial object's coordinate units. Ensures
#'              that the CRS matches the feature geometry units. 
#' 
#' @param obj sf object; The geospatial object to be corrected.
#'
#' @details Geospatial features generated using the GeoJSON format often 
#' contain a CRS that does not match the feature geometry coordinate units. 
#' The GeoJSON format made the choice to not even try: 
#' [RFC 7946 - The GeoJSON Format](https://datatracker.ietf.org/doc/html/rfc7946#page-12).
#' 
#' @returns the geospatial object with CRS correctly defined to match feature 
#' coordinate geometry. 
#' @export
#' 
#' @importFrom sf st_crs st_bbox st_set_crs
#' @importFrom assertthat assert_that
sf_fix_crs <- function(obj) {
  assert_that(any(class(obj) %in% c("sf")), 
              msg = "obj must be an sf object")
  assert_that(any(st_crs(obj)$epsg %in% c(3857, 4326)),
              msg = "obj must have a crs of either 3857 or 4326")
  
  bb_obj <- st_bbox(obj)
  bb_4326 <- st_bbox(c(xmin = -180,
                       ymin = -90,
                       xmax =  180,
                       ymax =  90))
  bb_3857 <- st_bbox(c(xmin = -20037508.34,
                       ymin = -20048966.10,
                       xmax =  20037508.34,
                       ymax =  20048966.10))
  
  # Inside 4326?
  xmin_in_4326 <- bb_4326$xmin <= bb_obj$xmin && bb_obj$xmin <= bb_4326$xmax
  xmax_in_4326 <- bb_4326$xmin <= bb_obj$xmax && bb_obj$xmax <= bb_4326$xmax
  ymin_in_4326 <- bb_4326$ymin <= bb_obj$ymin && bb_obj$ymin <= bb_4326$ymax
  ymax_in_4326 <- bb_4326$ymin <= bb_obj$ymax && bb_obj$ymax <= bb_4326$ymax
  in_4326 <- all(xmin_in_4326, xmax_in_4326, ymin_in_4326, ymax_in_4326)
  
  # Outside 4326?
  xmin_out_4326 <- bb_obj$xmin > bb_4326$xmin | bb_obj$xmin < bb_4326$xmax
  xmax_out_4326 <- bb_obj$xmax > bb_4326$xmax | bb_obj$xmax < bb_4326$xmax
  ymin_out_4326 <- bb_obj$ymin > bb_4326$ymin | bb_obj$ymin < bb_4326$ymax
  ymax_out_4326 <- bb_obj$ymax > bb_4326$ymax | bb_obj$ymax < bb_4326$ymin
  out_4326 <- all(xmin_out_4326, xmax_out_4326, ymin_out_4326, ymax_out_4326)
  
  # 4326
  if(in_4326 == TRUE & out_4326 == FALSE) {
    suppressWarnings(
      obj <- st_set_crs(obj, 4326)
    )
  } 
  # 3857
  if(in_4326 == FALSE & out_4326 == TRUE) {
    suppressWarnings(
      obj <- st_set_crs(obj, 3857)
    )
  }
  
  return(obj)
}