#' @title Get Results Leaflet
#'
#' @param fl               sf object; A flowline object.
#' @param xs               sf object; A cross section object.
#' @param dem              terra SpatRast object; A DEM raster.
#' @param channel_poly     sf object; A channel polygon.
#' @param floodplain_poly  sf object; A floodplain poly object.
#'
#' @returns a leaflet object
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom leaflet addPolylines addPolygons
#' @importFrom leafem updateLayersControl
#' @importFrom sf st_transform
#'
get_results_leaflet <- function(fl, xs, dem,
                                channel_poly = NULL, floodplain_poly = NULL) {
  assert_that("sf" %in% class(fl),
              msg = "fl must be an sf object")
  assert_that("sf" %in% class(xs),
              msg = "xs must be an sf object")
  assert_that("SpatRaster" %in% class(dem),
              msg = "dem must be a SpatRaster object")
  assert_that("sf" %in% class(channel_poly),
              msg = "channel_poly must be an sf object")
  assert_that("sf" %in% class(floodplain_poly),
              msg = "floodplain_poly must be an sf object")
  assert_that(check_crs_3857(fl), msg = "fl must be crs 3857")
  assert_that(check_crs_3857(xs), msg = "xs must be crs 3857")
  assert_that(check_crs_3857(dem), msg = "dem must be crs 3857")
  assert_that(check_crs_3857(channel_poly),
              msg = "channel_poly must be crs 3857")
  assert_that(check_crs_3857(floodplain_poly),
              msg = "floodplain_poly must be crs 3857")

  results_leaflet <-
    get_terrain_leaflet(xs, dem) %>%
    addPolylines(
      data = st_transform(fl, crs = 4326),
      layerId = "flowline",
      color = "blue", weight = 1,
      group = "Flowline") %>%
    addPolygons(
      data = st_transform(channel_poly, crs = 4326),
      layerId = "channel_poly",
      color = "navy", weight = 1,
      group = "Channel") %>%
    addPolygons(
      data = st_transform(floodplain_poly, crs = 4326),
      layerId = "floodplain_poly",
      color = "forestgreen", weight = 1,
      group = "Floodplain") %>%
    updateLayersControl(
      addOverlayGroups = c("Elevation", "Cross Sections", "Flowline",
                           "Channel", "Floodplain"),
      position = "topleft")

  return(results_leaflet)
}
