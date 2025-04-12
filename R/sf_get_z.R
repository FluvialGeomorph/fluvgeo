#' @title SF Get Z
#' @description Adds a new attribute "Z" to the input points sf object based
#' on the input dem.
#' @param points sf object;
#' @param dem    terra SpatRast object;
#'
#' @returns point sf object with new field containing dem z
#' @export
#'
#' @importFrom fluvgeo sf_point_attributes
#' @importFrom terra vect extract
#' 
sf_get_z <- function(points, dem) {
  assert_that(st_crs(points) == st_crs(dem), 
              msg = "points and dem must be have the same crs")
  # Get the dem band names
  dem_bands <- names(dem)
  # Extract the dem values at points
  pts_extract <- extract(x = dem, y = vect(points), xy = TRUE)
  # Rename the z field
  pts_z <- pts_extract %>% rename(z = all_of(dem_bands[1]))
  
  return(pts_z)
}