#' @title Get Leaflet Map
#'
#' @description Creates a leaflet map.
#' @param search logical; Display a search bar?
#' @param zoom   numeric; Inital zoom level.
#'
#' @return a leaflet map object
#' @export
#' @importFrom leaflet leaflet setView addProviderTiles addLayersControl
#'                     addScaleBar addMeasure
#' @importFrom leafem addMouseCoordinates
#' @importFrom leaflet.extras addSearchOSM searchOptions
get_leaflet <- function(search = FALSE, zoom = 5) {

  leaflet_map <-
    leaflet() %>%
    addProviderTiles("USGS.USTopo", group = "USGS Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    addLayersControl(
      baseGroups = c("USGS Topo", "Imagery"),
      position = "topleft") %>%
    addMouseCoordinates() %>%
    addScaleBar(position = "bottomleft") %>%
    addMeasure(position = "bottomleft")

  leaflet_search_map <-
    leaflet_map %>%
    setView(lng = -93.85,
            lat = 37.45, zoom = zoom) %>%
    addSearchOSM(
      options = searchOptions(
        collapsed = TRUE,
        autoCollapse = TRUE,
        autoCollapseTime = 20000,
        minLength = 3,
        hideMarkerOnCollapse = TRUE,
        zoom = 14
      )
    )

  return(
    if (search) leaflet_search_map
    else leaflet_map
  )
}
