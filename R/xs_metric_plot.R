#' @title Plot cross section metrics
#'
#' @description Produces a longitudinal plot of cross section metrics for the
#' input stream reach.
#'
#' @export
#' @param metric          FluvialGeomorphicMetric object; the fluvial geomorphic
#'                        metric to be mapped
#' @param reach_xs_dims   data frame; a data frame of cross section
#'                        dimensions.
#' @param label_xs        boolean; Draw the cross section locations?
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{xs_metrics_plot} function requires a \code{xs_dimensions}
#' dataframe. See the \code{sin_xs_dimensions} package dataset for an
#' example of this format of cross section data produced by the
#' \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @examples
#' # Extract data from the fluvgeo::sin_xs_dimensions SpatialPointsDataFrame
#' sin_xs_dims_df <- fluvgeo::sin_xs_dimensions@@data
#'
#' # Call the xs_plot function
#' sin_profile <- xs_metrics_plot(reach_xs_dims = sin_xs_dims_df)
#'
#' # Print the graph
#' sin_profile
#'
#' @importFrom utils head tail
#' @importFrom rlang parse_expr .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs
#' @importFrom scales rescale
#'
xs_metric_plot <- function(metric, reach_xs_dims, label_xs = TRUE) {
  # Convert the metric variable into an expression
  metric_string <- rlang::parse_expr(paste0(".data$", metric@variable))

  # Gather data by metrics for plotting
  xs_dims <- gather(reach_xs_dims,
                    key = "metrics",
                    value = "values",
                    !!metric_string)

  # Set the threshold values used for drawing horizontal lines. Remove the
  # first and last threshold values (min and max) leaving the middle values
  metric_threshold_lines <- tail(head(metric@threshold_breaks, -1), -1)

  # Create a new variable to hold the metric labels
  xs_dims$metric_labels <- cut(xs_dims$values,
                               breaks = metric@threshold_breaks,
                               lables = metric@threshold_labels,
                               ordered_result = TRUE)

  # Draw the graph
  p <- ggplot(xs_dims,
              aes(x = .data$km_to_mouth,
                  y = .data$values,
                  color = .data$metric_labels,
                  label = .data$Seq)) +
    geom_point(size = 4) +
    scale_color_manual(values = fluvgeo::metric_colors(metric),
                       drop = FALSE,
                       labels = metric@threshold_labels) +
    geom_hline(yintercept = metric_threshold_lines,
               color = "red") +
    scale_x_reverse() +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
    labs(title = unique(metric@metric),
         x     = "Kilometers",
         y     = metric@metric)

  # Draw cross section labels
  xs_labels <- geom_text_repel(size = 1.8, color = "black")

  # Return the plot
  if(label_xs == FALSE) return(p)
  if(label_xs == TRUE)  return(p + xs_labels)
}
