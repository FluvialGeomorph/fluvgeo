#' @title Plot Level 1 Cross Section Metrics
#'
#' @description Produces a longitudinal plot of cross section metrics for the
#' input stream reach.
#'
#' @export
#' @param xs_dims_sf      SimpleFeatures data frame of Level 1 cross section
#'                        dimensions.
#' @param features_sf     SimpleFeatures data frame of infrastructure features.
#' @param label_xs        logical; Draw the cross section locations?
#' @param xs_label_freq   numeric; An integer indicating the frequency of
#'                        cross section labels.
#' @param profile_units   character; the units of the longitudinal profile.
#'                        One of "kilometers", "meters", "miles", or "feet"
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{xs_metrics_plot} function requires a \code{xs_dimensions}
#' SimpleFeatures data frame.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs vars
#'
xs_metrics_plot_L1 <- function(xs_dims_sf,
                               features_sf,
                               label_xs = TRUE,
                               xs_label_freq = 10,
                               profile_units = "kilometers") {
  # Check parameters
  check_cross_section_dimensions(xs_dims_sf, "level_1")
  check_features(features_sf)

  # Calculate a unit conversion coefficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Define `metrics` factor levels
  metrics_levels <- c("Watershed_Area_SqMile",
                      "Z",
                      "Z_smooth",
                      "slope",
                      "sinuosity")

  # Define `metrics` factor labels
  km_to_mouth_label <- paste0("Longitudinal Distance (", profile_units, ")")
  metrics_labels <- c("Watershed Area (sq mile)",
                      "Elevation (NAVD88 ft)",
                      "Smoothed Elevation (NAVD88 ft)",
                      "Slope",
                      "Sinuosity")

  # Create a metrics variable to control which facet receives feature labels
  features_sf$metrics <- factor(rep("sinuosity",
                                    length(features_sf$Name)),
                                levels = metrics_levels,
                                labels = metrics_labels)

  # Determine min y value
  plot_min_y <- min(na.omit(xs_dims_sf$sinuosity))

  # Gather data by metrics for plotting
  xs_dims <- tidyr::gather(xs_dims_sf,
                           key = "metrics",
                           value = "values",
                           na.rm = TRUE,
                           .data$Watershed_Area_SqMile,
                           .data$Z,
                           .data$Z_smooth,
                           .data$slope,
                           .data$sinuosity)

  # Set factor levels to control labeling
  xs_dims$metrics <- factor(xs_dims$metrics,
                            levels = metrics_levels,
                            labels = metrics_labels)

  # Define metric colors by `metrics_labels`. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette - Moonrise Kingdom
  # using names from `colors()`.
  metrics_cols <- c("Watershed Area (sq mile)"       = "coral3",
                    "Elevation (NAVD88 ft)"          = "darkgoldenrod4",
                    "Smoothed Elevation (NAVD88 ft)" = "mediumpurple4",
                    "Slope"                          = "indianred4",
                    "Sinuosity"                      = "darkolivegreen")

  # Determine cross section label frequency
  labeled_xs <- ((xs_dims$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- xs_dims[labeled_xs, ]

  # Draw the graph
  p <- ggplot(xs_dims,
              aes(x = .data$km_to_mouth * unit_coef,
                  y = .data$values,
                  color = .data$metrics,
                  label = .data$Seq)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_color_manual(values = metrics_cols) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey", size = 0.1)) +
    facet_grid(rows = vars(.data$metrics),
               labeller = label_wrap_gen(width = 15),
               scales = "free") +
    labs(title = unique(xs_dims$ReachName),
         x     = profile_units,
         y     = "") +
    geom_text_repel(inherit.aes = FALSE,
                    data = features_sf,
                    aes(x = .data$km_to_mouth * unit_coef,
                        y = rep(plot_min_y - 0, length(.data$Name)),
                        label = .data$Name),
                    nudge_x = 0, angle = 90, size = 3,
                    force = 0.01,
                    segment.size = 0)

  # Draw cross section labels
  xs_labels <- geom_text_repel(inherit.aes = FALSE,
                               data = xs_labels_sf,
                               aes(x = .data$km_to_mouth * unit_coef,
                                   y = .data$values,
                                   label = .data$Seq),
                               size = 3, color = "black")

  # Return the plot
  if(label_xs == FALSE) return(p)
  if(label_xs == TRUE)  return(p + xs_labels)
}
