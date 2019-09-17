#' @title Reach Metric Map
#'
#' @description Produces a reach fluvial geomorphic metric thematic map
#'   displaying the metric values at each cross section.
#'
#' @export
#' @param metric              MetricThreshold object; the fluvial geomorphic
#'                            metric to be mapped
#' @param flowline            SpatialLinesDataFrame; a flowline feature class
#' @param xs_dimensions       SpatialLinesDataFrame; a cross section dimensions
#'                            feature class
#'
#' @return a tmap object
#'
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols tm_text tm_compass
#' tm_scale_bar tm_layout
#'
reach_metric_map <- function(metric, flowline, xs_dimensions) {


  # Create the reach map
  metric_map <- tm_shape(shp = flowline,
                         bbox = bb(flowline, 1.1),
                         name = "Flowline") +
                  tm_lines(col = "blue", lwd = 2) +
                tm_shape(shp = xs_dimensions,
                         name = "Cross Sections") +
                  tm_symbols(col = metric@variable,
                             title.col = metric@metric,
                             size = 2,
                             palette = fgm::metric_colors(metric),
                             style = "fixed",
                             breaks = metric@threshold_breaks,
                             interval.closure = "left") +
                  tm_text(text = "Seq",
                          col = "black",
                          size = 0.5,
                          remove.overlap = TRUE) +
                tm_layout(legend.outside = TRUE,
                          legend.outside.position = "right",
                          frame.lwd = 3)
  return(metric_map)
}

