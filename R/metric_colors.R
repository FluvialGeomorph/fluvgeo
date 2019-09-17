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
