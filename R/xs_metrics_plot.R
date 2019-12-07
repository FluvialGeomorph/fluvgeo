#' @title Plot cross section metrics
#'
#' @description Produces a longitudinal plot of cross section metrics for the
#' input stream reach.
#'
#' @export
#' @param reach_xs_dims   data frame; a data frame of cross section
#'                        dimensions.
#' @param features_sp     SpatialPointsDataFrame of infrastructure features
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
#' # Extract cross section dimension data
#' sin_xs_dims_df <- fluvgeo::sin_riffle_floodplain_dims_planform_sp@@data
#'
#' # Call the xs_metrics_plot function
#' sin_metrics <- xs_metrics_plot(reach_xs_dims = sin_xs_dims_df)
#'
#' # Print the graph
#' print(sin_metrics)
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs vars
#'
xs_metrics_plot <- function(reach_xs_dims, features_sp, label_xs = TRUE) {
  # Check parameters
  check_cross_section_dimensions(reach_xs_dims, "metric_ratios")
  check_features(features_sp)

  # Convert to data frames for ggplot
  features <- features_sp@data

  # Define `metrics` factor levels
  metrics_levels = c("xs_width_depth_ratio",
                     "xs_entrenchment_ratio",
                     "slope",
                     "sinuosity",
                     "shear_stress",
                     "unit_stream_power",
                     "rc_bfw_ratio_10")

  # Define `metrics` factor labels
  metrics_labels = c("Width Depth Ratio",
                     "Entrenchment Ratio",
                     "Slope",
                     "Sinuosity",
                     "Shear Stress",
                     "Unit Stream Power",
                     "RC to BFW")

  # Create a metrics variable to control which facet receives feature labels
  features$metrics <- factor(rep("rc_bfw_ratio_10", length(features$Name)),
                             levels = metrics_levels,
                             labels = metrics_labels)

  # Determine min y value
  plot_min_y <- min(reach_xs_dims$rc_bfw_ratio_10)

  # Gather data by metrics for plotting
  xs_dims <- gather(reach_xs_dims,
                    key = "metrics",
                    value = "values",
                    .data$xs_width_depth_ratio,
                    .data$xs_entrenchment_ratio,
                    .data$slope,
                    .data$sinuosity,
                    .data$shear_stress,
                    .data$unit_stream_power,
                    .data$rc_bfw_ratio_10)

  # Set factor levels to control labelling
  xs_dims$metrics <- factor(xs_dims$metrics,
                            levels = metrics_levels,
                            labels = metrics_levels)

  # Define metric colors by `metrics_labels`. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette - Moonrise Kingdom
  # using names from colors().
  metrics_cols <- c("Width Depth Ratio"  = "coral3",
                    "Entrenchment Ratio" = "darkslategray4",
                    "Slope"              = "darkgoldenrod4",
                    "Sinuosity"          = "mediumpurple4",
                    "Shear Stress"       = "indianred4",
                    "Unit Stream Power"  = "darkolivegreen",
                    "RC to BFW"          = "plum4")

  # Draw the graph
  p <- ggplot(xs_dims,
              aes(x = .data$km_to_mouth,
                  y = .data$values,
                  color = .data$metrics,
                  label = .data$Seq)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    scale_color_manual(values = metrics_cols) +
    scale_x_reverse() +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
    facet_grid(rows =  vars(.data$metrics),
               scales = "free") +
    labs(title = unique(reach_xs_dims$ReachName),
         x     = "Kilometers",
         y     = "") +
    geom_text_repel(inherit.aes = FALSE,
                    data = features,
                    aes(x = .data$km_to_mouth,
                        y = rep(plot_min_y - 0, length(.data$Name)),
                        label = .data$Name),
                    nudge_x = 0, angle = 90, size = 1.8,
                    force = 0.01,
                    segment.size = 0)

  # Draw cross section labels
  xs_labels <- geom_text_repel(size = 1.8, color = "black")

  # Return the plot
  if(label_xs == FALSE) return(p)
  if(label_xs == TRUE)  return(p + xs_labels)
}
