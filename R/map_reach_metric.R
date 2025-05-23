#' @title Reach Metric Map
#'
#' @description Produces a reach fluvial geomorphic metric thematic map
#'   displaying the metric values at each cross section.
#'
#' @export
#' @param metric              MetricThreshold object; the fluvial geomorphic
#'                            metric to be mapped
#' @param flowline_sf         sf object; a flowline feature class.
#' @param xs_dimensions_sf    sf object; a cross section dimensions feature
#'                            class.
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
#' # Define geomorphic metric
#' wdr <- new(Class = "FluvialGeomorphicMetric",
#'            metric = "Width Depth Ratio",
#'            definition = "bankfull width / bankfull depth",
#'            variable = "xs_width_depth_ratio",
#'            threshold_breaks = c(0, 10, 20, Inf),
#'            threshold_labels = c("Incised",
#'                                 "Stable",
#'                                 "Overwidened"),
#'            source = "Dunn & Leopold, 1978")
#'
#' # Create the reach metric map
#' wdr_map <- map_reach_metric(wdr,
#'                             fluvgeo::sin_flowline_sf,
#'                             fluvgeo::sin_riffle_floodplain_dims_L3_sf)
#' print(wdr_map)
#'
#' @importFrom sf st_crs st_transform st_as_sf st_as_sfc
#' @importFrom grDevices colorRampPalette gray.colors
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols tm_text tm_compass
#'             tm_legend tm_scalebar tm_layout tm_pos_out tm_scale_intervals
#'             opt_tm_text
#' @importFrom terrainr get_tiles
#' @importFrom terra terrain shade rast
#'
map_reach_metric <- function(metric, flowline_sf, xs_dimensions_sf,
                             xs_label_freq = 2,
                             background = "none",
                             exaggeration = 20,
                             extent_factor = 1.1) {
  # Check data structure
  check_flowline(flowline_sf, step = "create_flowline")
  check_cross_section_dimensions(xs_dimensions_sf, step = "stream_power")

  # Reproject features to LatLong
  flowline_sf_ll      <- sf::st_transform(flowline_sf,
                                          crs = sf::st_crs("EPSG:4326"))
  xs_dimensions_sf_ll <- sf::st_transform(xs_dimensions_sf,
                                          crs = sf::st_crs("EPSG:4326"))

  # Set extent
  xs_extent <- fluvgeo::map_extent(xs_dimensions_sf_ll,
                                   extent_factor = extent_factor)
  xs_extent_poly <- sf::st_as_sf(sf::st_as_sfc(xs_extent))

  # Determine cross section label frequency
  labeled_xs <- ((xs_dimensions_sf$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- xs_dimensions_sf_ll[labeled_xs, ]

  # Specify legend position
  legend_pos <- tm_pos_out(cell.h = "right",
                           cell.v = "center",
                           pos.v = "top",
                           pos.h = "left")

  # Create the reach metric map
  # tmap_design_mode()
  metric_map <-
    tm_shape(shp = flowline_sf_ll,
             bbox = xs_extent,
             name = "Flowline") +
      tm_lines(col = "blue",
               lwd = 2) +
    tm_shape(shp = xs_dimensions_sf_ll,
             name = "Cross Sections") +
      tm_lines(col = "red3",
               lwd = 1) +
      tm_symbols(fill = metric@variable,
                 size = 2,
                 fill.scale = tm_scale_intervals(
                               breaks = metric@threshold_breaks,
                               values = fluvgeo::metric_colors(metric)),
                 fill.legend = tm_legend(
                                title = metric@metric,
                                position = legend_pos,
                                frame = FALSE)) +
    tm_shape(shp = xs_labels_sf,
             name = "Cross Section labels") +
      tm_text(text = "Seq",
              col = "black",
              size = 0.7,
              options = opt_tm_text(remove_overlap = TRUE)) +
    tm_compass(type = "arrow",
               position = c("left", "bottom")) +
    tm_scalebar(width = 25,
                position = c("right", "bottom")) +
    tm_layout(outer.margins = c(0, 0, 0, 0),
              meta.margins = c(0., 0, 0, 0.3),
              outer.bg.color = "white",
              frame.lwd = 3)
  metric_map

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

    background_map <- tm_shape(aerial_photos) +
                        tm_rgb()

    overview_map <- background_map + metric_map
  }

  # Elevation
  if(background == "elevation") {
    # Get elevation
    elev_tiles <- terrainr::get_tiles(xs_extent_poly,
                                      services = "elevation",
                                      resolution = 3)
    elevation <- terra::rast(elev_tiles[[1]])

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
    slp <- terra::terrain(exaggerated, v = "slope", unit = "radians")
    asp <- terra::terrain(exaggerated, v = "aspect", unit = "radians")
    hill_270 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 270)
    hill_315 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 315)
    hill_355 <- terra::shade(slope = slp, aspect = asp,
                             angle = 30, direction = 355)


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
                legend.show = TRUE)

    overview_map <- background_map + metric_map
  }

  # No background
  if(background == "none") {
    overview_map <- metric_map
  }

  return(overview_map)
}

