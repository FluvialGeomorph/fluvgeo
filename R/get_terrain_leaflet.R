#' Get a Terrain Leaflet
#'
#' @description Get a terrain map as a leaflet object.
#'
#' @param xs        sf object, The cross sections
#' @param dem       SpatRaster object, The digital elevation model.
#'
#' @return a leaflet object
#' @export
#'
#' @importFrom leaflet colorNumeric addRasterImage addPolylines labelOptions
#'                     addLabelOnlyMarkers
#' @importFrom leafem updateLayersControl
#' @importFrom leaflegend addLegendNumeric
#' @importFrom fluvgeo map_extent
#' @importFrom assertthat assert_that
#' @importFrom sf st_as_sf st_as_sfc st_bbox st_centroid st_transform
#'             st_geometry
#' @importFrom terra ext crop terrain shade minmax values
#'
get_terrain_leaflet <- function(xs, dem) {
  assert_that("sf" %in% class(xs),
               msg = "must be sf object")
  assert_that("SpatRaster" %in% class(dem),
              msg = "must be SpatRaster object")
  assert_that(check_crs_3857(xs), msg = "xs must be crs 3857")
  assert_that(check_crs_3857(dem), msg = "dem must be crs 3857")

  # set extent
  xs_extent <- fluvgeo::map_extent(xs, extent_factor = 1.2)
  xs_extent_poly <- sf::st_as_sf(sf::st_as_sfc(xs_extent))

  # Crop the dem to the cross section map extent (+ 20 pixels)
  crop_extent <- terra::ext(xs_extent_poly) + 20
  dem_i <- terra::crop(x = dem, y = crop_extent)

  # Calculate terrain variables
  terrain <- terra::terrain(dem_i, v = c("slope", "aspect"), unit = "radians")
  hill <- terra::shade(terrain$slope, terrain$aspect, normalize = TRUE)

  leaflet_hillshade <- colorNumeric(
    palette = "Greys",
    domain = as.vector(minmax(hill)),
    na.color = "transparent",
    alpha = TRUE,
    reverse = TRUE)

  # Create a leaflet topo color mapping
  leaflet_topo <- colorNumeric(
    palette = c("cadetblue2", "khaki1", "chartreuse4", "goldenrod1",
                "orangered4", "saddlebrown", "gray70", "white"),
    domain = as.vector(minmax(dem_i)),
    na.color = "transparent",
    alpha = TRUE,
    reverse = FALSE)

  dem_bbox <- sf::st_bbox(crop_extent)

  terrain_leaflet <-
    get_leaflet(
      search = FALSE) %>%
    addRasterImage(
      x = hill,
      colors = leaflet_hillshade,
      group = "Elevation",
      opacity = 0.5, project = FALSE) %>%
    addRasterImage(
      x = dem_i,
      colors = leaflet_topo,
      group = "Elevation",
      opacity = 0.5, project = FALSE) %>%
    addLegendNumeric(
      pal = leaflet_topo,
      values = values(dem_i),
      title = "NAVD88, ft",
      decreasing = FALSE,
      group = "Elevation",
      position = "topright") %>%
    addPolylines(
      data = st_transform(xs, crs = 4326),
      color = "black",
      group = "Cross Sections") %>%
    addLabelOnlyMarkers(
      data = st_centroid(st_geometry(st_transform(xs, crs = 4326))),
      label = xs$Seq,
      group = "Cross Sections",
      labelOptions = labelOptions(noHide = TRUE, direction = 'top',
                                  textsize = "14px", textOnly = TRUE)) %>%
    updateLayersControl(
      addOverlayGroups = c("Elevation", "Cross Sections"),
      position = "topleft")

  #terrain_leaflet
  return(terrain_leaflet)
}
