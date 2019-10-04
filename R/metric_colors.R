#' @title Metric colors
#'
#' @description Returns a vector of colors based on the number of threshold
#'   breaks in the input `FluvialGeomorphicMetric`` object.
#'
#' @export
#' @param metric              MetricThreshold object; the fluvial geomorphic
#'                            metric to be mapped
#'
#' @return a vector of colors
#'
#' @examples
#' # Define metric objects
#'wdr3 <- new(Class = "FluvialGeomorphicMetric",
#'            metric = "Width Depth Ratio",
#'            definition = "bankfull width / bankfull depth",
#'            variable = "xs_width_depth_ratio",
#'            threshold_breaks = c(0, 10, 20, Inf),
#'            threshold_labels = c("Incised",
#'                                 "Stable",
#'                                 "Overwidened"),
#'            source = "Dunn & Leopold, 1978")
#'
#' # Get a set of colors for a `FluvialGeomorphicMetric` object
#' cols <- metric_colors(wdr3)
#'
metric_colors <- function(metric) {
  #determine the number of colors needed
  num_of_colors <- as.character(length(metric@threshold_labels))

  # Calculate colors
  switch(num_of_colors,
         "3" = {colors <- c("coral3", "wheat", "cadetblue3")
                names(colors) <- levels(metric@threshold_labels)},
         "4" = {colors <- c("coral3", "coral", "cadetblue2", "cadetblue")
                names(colors) <- levels(metric@threshold_labels)},
         stop(paste("No colors defined for",
                    num_of_colors,
                    "number of metric breaks. "))
  )
  return(colors)
}
