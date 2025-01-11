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
#' @importFrom grDevices colorRampPalette gray.colors
#' @importFrom tmap tm_shape tm_raster tm_lines tm_text tm_scale_continuous
#'             tm_layout tm_legend tm_pos_out opt_tm_text
#'             tm_compass tm_scalebar tm_title_out
#' @importFrom terrainr get_tiles
#' @importFrom terra shade terrain rast
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
                                      crs = sf::st_crs("EPSG:4326"))
  cross_section_sf_ll <- sf::st_transform(cross_section_sf,
                                      crs = sf::st_crs("EPSG:4326"))

  # Set extent
  xs_extent <- fluvgeo::map_extent(cross_section_sf_ll,
                                       extent_factor = extent_factor)
  xs_extent_poly <- sf::st_as_sf(sf::st_as_sfc(xs_extent))

  # Determine cross section label frequency
  labeled_xs <- ((cross_section_sf$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- cross_section_sf_ll[labeled_xs, ]

  # Create thematic layers
  # tmap_design_mode()
  thematic_map <-
    tm_shape(shp = flowline_sf_ll,
             name = "Flowline",
             unit = "mi",
             bbox = xs_extent,
             is.main = TRUE) +
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
            options = opt_tm_text(remove_overlap = TRUE)) +
    tm_compass(type = "arrow",
               position = c("right", "bottom")) +
    tm_scalebar(width = 25,
                position = c("left", "bottom")) +
    tm_title_out(text = unique(flowline_sf_ll$ReachName),
                 fontface = "bold",
                 padding = c(0, 0.01, 0, 0)) +
    tm_layout(outer.margins = c(0, 0, 0, 0),
              meta.margins = c(0, 0, 0.03, 0),
              outer.bg.color = "white",
              frame.lwd = 3,
              saturation = 1.2)
  #thematic_map

  # Aerial
  if(background == "aerial") {
    # Get aerial photos
    aerial_photos <- mapboxapi::get_static_tiles(
      location = xs_extent_poly,
      zoom = 15,
      style_id = "satellite-streets-v12",
      style_url = "mapbox://styles/mapbox/satellite-streets-v12",
      scaling_factor = "2x",
      buffer_dist = 0,
      crop = TRUE,
      username = "mikedoc",
      access_token = "pk.eyJ1IjoibWlrZWRvYyIsImEiOiJja2VwcThtcm4wbHMxMnJxdm1wNjE5eXhmIn0.WE_PG_GiKhpqr6JIJbTsmQ")

    background_map <- tm_shape(aerial_photos, raster.downsample = TRUE) +
                        tm_rgb()
    overview_map <- background_map + thematic_map
    #overview_map
  }

  # Elevation
  if(background == "elevation") {
    # Get elevation
    elev_tiles <- terrainr::get_tiles(xs_extent_poly,
                                      services = "elevation",
                                      resolution = 3)
    elevation <- terra::rast(elev_tiles[[1]])

    # Convert elevation meters to feet
    elev_ft <- elevation * 3.28084

    # Create a hillshade
    exaggerated <- elevation * exaggeration
    slp <- terra::terrain(exaggerated, v = "slope", unit = "radians")
    asp <- terra::terrain(exaggerated, v = "aspect", unit = "radians")
    hill_270 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 270)
    hill_315 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 315)
    hill_355 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 355)

    # Create an esri-like topo color ramp
    esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                        "chartreuse4", "goldenrod1",
                                                        "orangered4", "saddlebrown",
                                                        "gray70", "white"),
                                             bias = 1,
                                             space = "Lab",
                                             interpolate = "linear")

    # Specify legend position
    legend_pos <- tm_pos_out(cell.h = "right",
                             cell.v = "center",
                             pos.v = "top",
                             pos.h = "left")

    background_map <-
      tm_shape(shp = hill_270,
               name = "Hillshade") +
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.grays"),
                col.legend = tm_legend(show = FALSE),
                col_alpha = 1) +
      tm_shape(shp = hill_315,
               name = "Hillshade 315") +
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.grays"),
                col.legend = tm_legend(show = FALSE),
                col_alpha = 0.5) +
      tm_shape(shp = hill_355,
               name = "Hillshade 355") +
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.grays"),
                col.legend = tm_legend(show = FALSE),
                col_alpha = 0.5) +
      tm_shape(shp = elev_ft,
               name = "Elevation",
               unit = "ft") +
      tm_raster(col.scale = tm_scale_continuous(values = esri_topo(1000)),
                col_alpha = 0.6,
                col.legend = tm_legend(
                  title = "Elevation \n(NAVD88, ft)",
                  reverse = TRUE,
                  frame = FALSE,
                  position = legend_pos)) +
      tm_layout(legend.outside = TRUE)

    overview_map <- background_map + thematic_map
    #overview_map
  }

  # No background
  if(background == "none") {
    overview_map <- thematic_map
  }

  return(overview_map)
}

