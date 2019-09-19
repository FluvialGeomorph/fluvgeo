#' @title Reach Overview Map
#'
#' @description Produces a reach overview map displaying cross section
#' locations over an aerial image.
#'
#' @export
#' @param flowline            SpatialLinesDataFrame; a flowline feature class
#' @param xs_dimensions       SpatialLinesDataFrame; a cross section dimensions
#'                            feature class
#'
#' @return a tmap object
#'
#' @importFrom tmaptools bb get_projection read_osm
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols
#' tm_text tm_compass tm_scale_bar tm_layout
#'
reach_overview_map <- function(flowline, xs_dimensions) {
  # Create map extent in lat-long to pass to OpenStreetMap
  map_bb <- tmaptools::bb(fgm::feature_extent(flowline),
                          current.projection = tmaptools::get_projection(flowline),
                          ext = 1.05,
                          projection = "longlat")

  # Create the reach map
  reach_map <- tm_shape(tmaptools::read_osm(map_bb,
                                   type = "bing")) +
                 tm_rgb() +
               tm_shape(shp = flowline,
                        name = "Flowline",
                        unit = "mi",
                        bbox = feature_extent(flowline, 1.05),
                        is.master = TRUE) +
                 tm_lines(col = "blue", lwd = 3) +
               tm_shape(shp = xs_dimensions,
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
