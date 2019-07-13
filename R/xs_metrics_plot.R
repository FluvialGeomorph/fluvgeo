#' @title Plot cross section metrics
#'
#' @description Produces a longitudinal plot of cross section metrics for the
#' input stream reach.
#'
#' @export
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
#' # Extract data from the fgm::sin_xs_dimensions SpatialPointsDataFrame
#' sin_xs_dims_df <- fgm::sin_xs_dimensions@@data
#'
#' # Call the xs_plot function
#' sin_profile <- xs_metrics_plot(reach_xs_dims = sin_xs_dims_df)
#'
#' # Print the graph
#' sin_profile
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs
#'
xs_metrics_plot <- function(reach_xs_dims, label_xs = TRUE) {
  # Gather data by metrics for plotting
  xs_dims <- gather(reach_xs_dims,
                    key = "metrics",
                    value = "values",
                    .data$xs_width_depth_ratio,
                    .data$xs_entrenchment_ratio,
                    .data$slope,
                    .data$sinuosity,
                    .data$bend_radius,
                    .data$meander_length,
                    .data$meander_width)

  # Set factor levels to control legend
  xs_dims$metrics <- factor(xs_dims$metrics,
                            levels = c("xs_width_depth_ratio",
                                       "xs_entrenchment_ratio",
                                       "slope",
                                       "sinuosity",
                                       "bend_radius",
                                       "meander_length",
                                       "meander_width"),
                            labels = c("Width Depth Ratio",
                                       "Entrenchment Ratio",
                                       "Slope",
                                       "Sinuosity",
                                       "Bend Radius",
                                       "Meander Length",
                                       "Meander Width"))

  # Define colors and labels. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette - Moonrise Kingdom
  # using names from colors().
  cols <- c("Width Depth Ratio"  = "coral3",
            "Entrenchment Ratio" = "darkslategray4",
            "Slope"              = "darkgoldenrod4",
            "Sinuosity"          = "mediumpurple4",
            "Bend Radius"        = "indianred4",
            "Meander Length"     = "darkolivegreen",
            "Meander Width"      = "plum4")

  # Draw the graph
  p <- ggplot(xs_dims,
              aes(x = .data$km_to_mouth,
                  y = .data$values,
                  color = .data$metrics, label = .data$Seq)) +
    geom_line(size = 2) +
    scale_color_manual(values = cols) +
    scale_x_reverse() +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
    facet_grid(facets = metrics ~ .,
               scales = "free") +
    labs(title = unique(reach_xs_dims$ReachName),
         x     = "Kilometers",
         y     = "")

  # Draw cross section labels
  xs_labels <- geom_text_repel(size = 1.8, color = "black")

  # Return the plot
  if(label_xs == FALSE) return(p)
  if(label_xs == TRUE)  return(p + xs_labels)
}
