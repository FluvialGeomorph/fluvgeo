#' @title Level 1 longitudinal metrics
#'
#' @description Produces a longitudinal plot of Level 1 metrics (water surface
#' elevation, slope, and sinuosity) for the input stream reach.
#'
#' @export
#' @param gradient_sf     sf data frame of gradient points.
#' @param features_sf     sf data frame of infrastructure features
#'
#' @return A ggplot2 object.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs vars label_wrap_gen
#'
flowline_metrics <- function(gradient_sf, features_sf) {
  # Check parameters
  fluvgeo::check_features(features_sf)

  # Convert to data frames for ggplot
  gradient <- gradient_sf
  features <- features_sf


  # Define `metrics` factor levels
  metrics_levels <- c("Z_smooth",
                      "slope",
                      "sinuosity")

  # Define `metrics` factor labels
  metrics_labels <- c("Water Surface Elevation (ft)",
                      "Slope",
                      "Sinuosity")

  # Create a metrics variable to control which facet receives feature labels
  features$metrics <- factor(rep("sinuosity", length(features$Name)),
                             levels = metrics_levels,
                             labels = metrics_labels)

  # Determine min y value
  plot_min_y <- min(gradient$sinuosity)

  # Gather data by metrics for plotting
  flowline <- gather(gradient,
                     key = "metrics",
                     value = "values",
                     .data$Z_smooth,
                     .data$slope,
                     .data$sinuosity)

  # Set factor levels to control labelling
  flowline$metrics <- factor(flowline$metrics,
                             levels = metrics_levels,
                             labels = metrics_labels)

  # Define metric colors by `metrics_labels`. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette - Moonrise Kingdom
  # using names from colors().
  metrics_cols <- c("Water Surface Elevation (ft)"= "coral3",
                    "Slope"                       = "darkgoldenrod4",
                    "Sinuosity"                   = "mediumpurple4")

  # Draw the graph
  p <- ggplot(flowline,
              aes(x = .data$POINT_M,
                  y = .data$values,
                  color = .data$metrics)) +
    geom_point(size = 0.5) +
    geom_line(size = 1) +
    scale_color_manual(values = metrics_cols) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
    facet_grid(rows = vars(.data$metrics),
               labeller = label_wrap_gen(width = 15),
               scales = "free") +
    labs(title = unique(flowline$ReachName),
         x     = "Kilometers",
         y     = "") +
    geom_text_repel(inherit.aes = FALSE,
                    data = features,
                    aes(x = .data$km_to_mouth,
                        y = rep(plot_min_y - 0, length(.data$Name)),
                        label = .data$Name),
                    nudge_x = 0, angle = 90, size = 3,
                    force = 0.01,
                    segment.size = 0)

  # Draw cross section labels
  xs_labels <- geom_text_repel(size = 1.8, color = "black")

  # Return the plot
  return(p)
}
