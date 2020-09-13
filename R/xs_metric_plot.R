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
#' @param label_xs        logical; Draw the cross section locations?
#' @param xs_label_freq   numeric; An integer indicating the frequency of
#'                        cross section labels.
#' @param profile_units   character; the units of the longitudinal profile.
#'                        One of "kilometers", "meters", "miles", or "feet"
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{xs_metrics_plot} function requires a \code{xs_dimensions}
#' dataframe. See the \code{sin_xs_dimensions} package dataset for an
#' example of this format of cross section data produced by the
#' \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @examples
#' # Get feature class test data
#' xs_planform_fc <- file.path(system.file("extdata", "testing_data.gdb",
#'                                         package = "fluvgeo"),
#'                             "riffle_floodplain_dims_planform")
#'
#' # Convert feature classes to an sf objects
#' xs_planform_sf   <- fluvgeo::fc2sf(xs_planform_fc)
#'
#' # Create the fluvgeo::FluvialGeomorphicMetric object
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
#' metric_plot <- fluvgeo::xs_metric_plot(metric = wdr,
#'                                        reach_xs_dims = xs_planform_sf,
#'                                        label_xs = TRUE,
#'                                        profile_units = "miles")
#' print(metric_plot)
#'
#' @importFrom utils head tail
#' @importFrom rlang parse_expr .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme labs
#' @importFrom scales rescale
#'
xs_metric_plot <- function(metric,
                           reach_xs_dims,
                           label_xs = TRUE,
                           xs_label_freq = 10,
                           profile_units = "kilometers") {
  # Convert the metric variable into an expression
  metric_string <- rlang::parse_expr(paste0(".data$", metric@variable))

  # Calculate a unit conversion coefficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Gather data by metrics for plotting
  xs_dims <- tidyr::gather(reach_xs_dims,
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

  # Determine cross section label frequency
  labeled_xs <- ((xs_dims$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- xs_dims[labeled_xs, ]

  # Draw the graph
  p <- ggplot(xs_dims,
              aes(x = .data$km_to_mouth * unit_coef,
                  y = .data$values,
                  color = .data$metric_labels,
                  label = .data$Seq)) +
    geom_point(size = 4) +
    scale_color_manual(values = fluvgeo::metric_colors(metric),
                       drop = FALSE,
                       labels = metric@threshold_labels) +
    geom_hline(yintercept = metric_threshold_lines,
               color = "red") +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey", size = 0.1)) +
    labs(title = unique(metric@metric),
         x     = profile_units,
         y     = metric@metric)

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
