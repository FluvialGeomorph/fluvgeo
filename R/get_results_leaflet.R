#' @title Get Results Leaflet
#'
#' @param fl    sf object;
#' @param xs    sf object;
#' @param dem   terra SpatRast object;
#'
#' @returns a leaflet object
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom leaflet addPolylines
#' @importFrom leafem updateLayersControl
#' @importFrom sf st_transform
#' 
get_results_leaflet <- function(fl, xs, dem) {
  assert_that("sf" %in% class(fl), 
              msg = "fl must be an sf object")
  assert_that("sf" %in% class(xs), 
              msg = "xs must be an sf object")
  assert_that("SpatRaster" %in% class(dem), 
              msg = "dem must be a SpatRaster object")
  assert_that(check_crs_3857(fl), msg = "fl must be crs 3857")
  assert_that(check_crs_3857(xs), msg = "xs must be crs 3857")
  assert_that(check_crs_3857(dem), msg = "dem must be crs 3857")
  
  results_leaflet <- 
    get_terrain_leaflet(xs, dem) %>%
    addPolylines(
      data = st_transform(fl, crs = 4326),
      color = "blue",
      group = "Flowline") %>%
    updateLayersControl(
      addOverlayGroups = c("Elevation", "Cross Sections", "Flowline"),
      position = "topleft")
  
  return(results_leaflet)
}