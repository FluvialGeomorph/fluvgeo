#' @title Reach Overview Map
#'
#' @description Produces a reach overview map displaying cross section
#' locations over an aerial image.
#'
#' @export
#' @param flowline            SpatialLinesDataFrame; a flowline feature class
#' @param cross_section       SpatialLinesDataFrame; a cross section feature
#'                            class
#'
#' @return a tmap object
#'
#' @examples
#' # Use the fluvgeo::sin_flowline_sp SpatialLinesDataFrame
#' sin_flowline_sp <- fluvgeo::sin_flowline_sp
#'
#' # Use the fluvgeo::sin_riffle_floodplain_sp SpatialLinesDataFrame
#' sin_riffle_channel_sp <- fluvgeo::sin_riffle_channel_sp
#'
#' # Create the map
#' sin_map <- map_reach_overview(sin_flowline_sp, sin_riffle_channel_sp)
#'
#' # Print the map
#' print(sin_map)
#'
#' @importFrom sf st_crs
#' @importFrom tmaptools bb read_osm
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols
#' tm_text tm_compass tm_scale_bar tm_layout
#'
map_reach_overview <- function(flowline, cross_section) {
  # Check data structure
  check_flowline(flowline, step = "create_flowline")
  check_cross_section(cross_section, step = "assign_ids")

  # Create map extent in lat-long to pass to OpenStreetMap
  map_bb <- tmaptools::bb(fluvgeo::feature_extent(flowline),
                          current.projection = sf::st_crs(flowline),
                          ext = 1.05,
                          projection = 4326)                     # longlat WGS84

  # Get basemap tiles {mapmisc}
  #basemap_tiles <- mapmisc::openmap(flowline)
  ## read_osm
  #basemap_tiles <- tmaptools::read_osm(flowline)

  # Create the reach map
  reach_map <- # tm_shape(tmaptools::read_osm(map_bb, type = "bing"),
               #          projection = sf::st_crs(flowline)) +
               # tm_shape(basemap_tiles) +
               #   tm_raster() +
               tm_shape(shp = flowline,
                        name = "Flowline",
                        unit = "mi",
                        bbox = feature_extent(flowline, 1.05),
                        is.master = TRUE) +
                 tm_lines(col = "blue", lwd = 3) +
               tm_shape(shp = cross_section,
                        name = "Cross Sections") +
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
               tm_layout(main.title = unique(flowline$ReachName),
                         frame.lwd = 3)
  return(reach_map)
}
