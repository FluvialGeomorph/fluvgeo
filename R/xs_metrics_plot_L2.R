#' @title Plot Level 2 Cross Section Metrics
#'
#' @description Produces a longitudinal plot of Level 2 cross section metrics
#' for the input stream reach.
#'
#' @export
#' @param xs_dims_sf      SimpleFeatures data frame of Level 2 cross section
#'                        dimensions.
#' @param features_sf     SimpleFeatures data frame of infrastructure features.
#' @param label_xs        logical; Draw the cross section locations?
#' @param xs_label_freq   numeric; An integer indicating the frequency of
#'                        cross section labels.
#' @param profile_units   character; the units of the longitudinal profile.
#'                        One of "kilometers", "meters", "miles", or "feet".
#'
#' @return A ggplot2 object.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs vars
#'
xs_metrics_plot_L2 <- function(xs_dims_sf,
                               features_sf,
                               label_xs = TRUE,
                               xs_label_freq = 10,
                               profile_units = "kilometers") {
  # Check parameters
  check_cross_section_dimensions(xs_dims_sf, "stream_power")
  check_features(features_sf)

  # Calculate a unit conversion coefficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Define `metrics` factor levels
  metrics_levels <- c("xs_width_depth_ratio_gte_one",
                      "xs_entrenchment_ratio_gte_one",
                      "slope_gte_zero",
                      "sinuosity_gte_one",
                      "shear_stress_weight_gte_zero",
                      "unit_stream_power_gte_zero")


  # Define `metrics` factor labels
  metrics_labels <- c("Width Depth Ratio",
                      "Entrenchment Ratio",
                      "Slope",
                      "Sinuosity",
                      "Shear Stress (lb/ft^2)",
                      "Unit Stream Power (kg/m/s)")


  # Create a metrics variable to control which facet receives feature labels
  features_sf$metrics <- factor(rep("unit_stream_power_gte_zero",
                                    length(features_sf$Name)),
                                levels = metrics_levels,
                                labels = metrics_labels)

  # Determine min y value
  plot_min_y <- min(na.omit(xs_dims_sf$unit_stream_power))

  # Gather data by metrics for plotting
  xs_dims <- tidyr::gather(xs_dims_sf,
                           key = "metrics",
                           value = "values",
                           na.rm = TRUE,
                           .data$xs_width_depth_ratio_gte_one,
                           .data$xs_entrenchment_ratio_gte_one,
                           .data$slope_gte_zero,
                           .data$sinuosity_gte_one,
                           .data$shear_stress_weight_gte_zero,
                           .data$unit_stream_power_gte_zero)


  # Set factor levels to control labeling
  xs_dims$metrics <- factor(xs_dims$metrics,
                            levels = metrics_levels,
                            labels = metrics_labels)

  # Define metric colors by `metrics_labels`. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette - Moonrise Kingdom
  # using names from `colors()`.
  metrics_cols <- c("Width Depth Ratio"           = "coral3",
                    "Entrenchment Ratio"          = "darkslategray4",
                    "Slope"                       = "darkgoldenrod4",
                    "Sinuosity"                   = "mediumpurple4",
                    "Shear Stress (lb/ft^2)"      = "indianred4",
                    "Unit Stream Power (kg/m/s)"  = "darkolivegreen")


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
