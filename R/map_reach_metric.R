#' @title Reach Metric Map
#'
#' @description Produces a reach fluvial geomorphic metric thematic map
#'   displaying the metric values at each cross section.
#'
#' @export
#' @param metric              MetricThreshold object; the fluvial geomorphic
#'                            metric to be mapped
#' @param flowline            SpatialLinesDataFrame or sf; a flowline feature
#'                            class.
#' @param xs_dimensions       SpatialLinesDataFrame or sf; a cross section
#'                            dimensions feature class.
#' @param xs_label_freq       numeric; An integer indicating the frequency of
#'                            cross section labels.
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
#'                             fluvgeo::sin_flowline_sp,
#'                             fluvgeo::sin_riffle_floodplain_dims_planform_sp)
#' print(wdr_map)
#'
#' @importFrom tmap tm_shape tm_rgb tm_lines tm_symbols tm_text tm_compass
#' tm_scale_bar tm_layout
#'
map_reach_metric <- function(metric, flowline, xs_dimensions,
                             xs_label_freq = 2,
                             extent_factor = 1.1) {
  # Check data structure
  check_flowline(flowline, step = "create_flowline")

  # Set extent
  xs_extent <- fluvgeo::feature_extent(xs_dimensions,
                                       extent_factor = extent_factor)

  # Determine cross section label frequency
  labeled_xs <- ((xs_dimensions$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- xs_dimensions[labeled_xs, ]

  # Create the reach map
  metric_map <- tm_shape(shp = flowline,
                         bbox = xs_extent,
                         name = "Flowline") +
                  tm_lines(col = "blue",
                           lwd = 2) +
                tm_shape(shp = xs_dimensions,
                         name = "Cross Sections") +
                  tm_lines(col = "red3",
                           lwd = 2) +
                  tm_symbols(col = metric@variable,
                             title.col = metric@metric,
                             size = 2,
                             palette = fluvgeo::metric_colors(metric),
                             style = "fixed",
                             breaks = metric@threshold_breaks,
                             interval.closure = "left") +
                tm_shape(shp = xs_labels_sf,
                         name = "Cross Section labels") +
                  tm_text(text = "Seq",
                          col = "black",
                          size = 0.7,
                          remove.overlap = TRUE) +
                tm_layout(legend.outside = TRUE,
                          legend.outside.position = "right",
                          frame.lwd = 3)
  return(metric_map)
}

