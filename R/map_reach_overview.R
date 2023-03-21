#' @title Reach Overview Map
#'
#' @description Produces a reach overview map displaying cross section
#' locations over an aerial image or elevation multi-direction shaded relief.
#'
#' @export
#' @param flowline_sf         sf object; A flowline feature class
#' @param cross_section_sf    sf object; A cross section feature class
#' @param xs_label_freq       numeric; An integer indicating the frequency of
#'                            cross section labels.
#' @param background          character; The type of map background. One of
#'                            "aerial", "elevation", or "none".
#' @param exaggeration        numeric; The degree of terrain exaggeration.
#' @param extent_factor       numeric; The amount the extent is expanded around
#'                            the cross section feature class. Values greater
#'                            than one zoom out, values less than one zoom in.
#'
#' @return a tmap object
#'
#' @importFrom sf st_crs st_transform st_bbox st_as_sfc
#' @importFrom sp CRS
#' @importFrom ceramic cc_location cc_elevation
#' @importFrom raster terrain hillShade
#' @importFrom grDevices colorRampPalette gray.colors
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols tm_text tm_compass
#' tm_scale_bar tm_layout
#' @importFrom maptiles get_tiles
#' @importFrom terrainr get_tiles
#' @importFrom terra shade terrain
#'
map_reach_overview <- function(flowline_sf, cross_section_sf,
                               background = "none",
                               xs_label_freq = 1,
                               exaggeration = 20,
                               extent_factor = 1.1) {
  # Check data structure
  check_flowline(flowline_sf, step = "create_flowline")
  check_cross_section(cross_section_sf, step = "assign_ids")

  # Reproject features to LatLong
  flowline_sf_ll      <- sf::st_transform(flowline_sf,
                                      crs = sp::CRS(SRS_string = "EPSG:4326"))
  cross_section_sf_ll <- sf::st_transform(cross_section_sf,
                                      crs = sp::CRS(SRS_string = "EPSG:4326"))

  # Set extent
  xs_extent <- fluvgeo::feature_extent(cross_section_sf_ll,
                                       extent_factor = extent_factor)
  # Create bbox
  sf_bbox<-sf::st_bbox(xs_extent,crs=sf::st_crs("EPSG:4326"))

  # Set Mapbox API key
  Sys.setenv(MAPBOX_API_KEY="pk.eyJ1IjoibWlrZWRvYyIsImEiOiJja2VwcThtcm4wbHMxMnJxdm1wNjE5eXhmIn0.WE_PG_GiKhpqr6JIJbTsmQ")

  # Determine cross section label frequency
  labeled_xs <- ((cross_section_sf$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- cross_section_sf_ll[labeled_xs, ]

  # Create thematic layers
  thematic_map <- tm_shape(shp = flowline_sf_ll,
                           name = "Flowline",
                           unit = "mi",
                           bbox = xs_extent,
                           is.master = TRUE) +
                    tm_lines(col = "blue",
                             lwd = 3) +
                  tm_shape(shp = cross_section_sf_ll,
                           name = "Cross Sections") +
                     tm_lines(col = "red3",
                              lwd = 3) +
                  tm_shape(shp = xs_labels_sf,
                           name = "Cross Section labels") +
                     tm_symbols(col = "white",
                                size = 1.0) +
                     tm_text(text = "Seq",
                             col = "black",
                             size = 0.5,
                             remove.overlap = TRUE) +
                  tm_compass(type = "arrow",
                             position = c("right", "bottom")) +
                  tm_scale_bar(width = 0.25,
                               position = c("left", "bottom")) +
                  tm_layout(main.title = unique(flowline_sf_ll$ReachName),
                            frame.lwd = 3,
                            saturation = 1.2)

  # Aerial
  if(background == "aerial") {
    # Get aerial photos
    aerial_photos<-maptiles::get_tiles(x=sf_bbox, provider="Esri.WorldImagery", crop=TRUE)


    background_map <- tm_shape(aerial_photos) +
                        tm_rgb()

    overview_map <- background_map + thematic_map
  }

  # Elevation
  if(background == "elevation") {
    # Get elevation
    elev_sfbbox<-sf::st_as_sfc(sf_bbox)
    elev_tiles<-terrainr::get_tiles(elev_sfbbox, services="elevation", resolution=3)
    elevation<-terra::rast(elev_tiles[[1]])

    # Create an esri-like topo color ramp
    esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                      "chartreuse4", "goldenrod1",
                                                      "orangered4", "saddlebrown",
                                                      "gray70", "white"),
                                             bias = 1,
                                             space = "Lab",
                                             interpolate = "linear")
    # Convert elevation meters to feet
    elev_ft <- elevation * 3.28084

    # Create a hillshade
    exaggerated <- elevation * exaggeration
    slp <- terra::terrain(exaggerated, v="slope", unit="radians")
    asp<- terra::terrain(exaggerated, v="aspect", unit="radians")
    hill_270<-terra::shade(slope=slp, aspect=asp,
                           angle=30, direction=270)
    hill_315 <- terra::shade(slope=slp, aspect=asp,
                             angle=30, direction=315)
    hill_355 <- terra::shade(slope=slp, aspect=asp,
                             angle=30, direction=355)
    #hill <- max(hill_270, hill_315, hill_355)

    background_map <- tm_shape(shp = hill_270,
                               name = "Hillshade") +
                        tm_raster(style = "cont",
                                  palette = gray.colors(100, 0, 1),
                                  alpha = 1,
                                  legend.show = FALSE) +
                      tm_shape(shp = hill_315,
                               name = "Hillshade 315") +
                        tm_raster(style = "cont",
                                  palette = gray.colors(100, 0, 1),
                                  alpha = 0.5,
                                  legend.show = FALSE) +
                      tm_shape(shp = hill_355,
                               name = "Hillshade 355") +
                        tm_raster(style = "cont",
                                  palette = gray.colors(100, 0, 1),
                                  alpha = 0.5,
                                  legend.show = FALSE) +
                      tm_shape(elev_ft,
                               name = "Elevation",
                               unit = "ft") +
                        tm_raster(style = "cont",
                                  palette = esri_topo(1000),
                                  alpha = 0.6,
                                  title = "Elevation (NAVD88, ft)",
                                  legend.show = TRUE)+
                        tm_layout(legend.outside= TRUE)

    overview_map <- background_map + thematic_map
  }

  # No background
  if(background == "none") {
    overview_map <- thematic_map
  }

  return(overview_map)
}

