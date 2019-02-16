#' @title Plot stream longitudinal profile
#'
#' @description Produces a longitudinal profile plot for the cross sections
#' of the input stream reach.
#'
#' @export
#' @param xs_dimensions   data frame; a data frame of cross section
#'                        dimensions.
#' @param features        data frame; a data frame of river features
#' @param label_xs        boolean; Draw the cross section locations?
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{profile_plot} function requires a \code{xs_dimensions}
#' dataframe. See the \code{\link{sin_xs_dimensions}} package dataset for an
#' example of this format of cross section data produced by the
#' \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @examples
#' # Extract data from the fgm::sin_xs_dimensions SpatialPointsDataFrame
#' sin_xs_dims_df <- fgm::sin_xs_dimensions@@data
#'
#' # Call the xs_plot function
#' sin_profile <- xs_profile_plot(xs_dimensions = sin_xs_dims_df)
#'
#' # Print the graph
#' sin_profile
#'
#' @importFrom assertthat assert_that
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw theme element_rect element_blank element_line labs
#'
#'
xs_profile_plot <- function(reach_xs_dims, features = NULL, label_xs = TRUE) {
  # Gather data by water levels for plotting
  xs_dims <- gather(reach_xs_dims,
                    key = "water_levels",
                    value = "elevations",
                    watersurface_elev, bankfull_elev, floodprone_elev)

  # Determine min y value
  plot_min_y <- min(xs_dims$elevations)

  # Set factor levels to control legend
  xs_dims$water_levels <- factor(xs_dims$water_levels,
                                 levels = c("floodprone_elev",
                                            "bankfull_elev",
                                            "watersurface_elev"),
                                 labels = c("Flood Prone",
                                            "Bankfull",
                                            "Water Surface"))

  # Create xs graphing data
  reach_xs_dims$elev_min <- reach_xs_dims$watersurface_elev - 2
  reach_xs_dims$elev_max <- reach_xs_dims$floodprone_elev + 2
  xs_lines <- gather(reach_xs_dims,
                     key = "elevations",
                     value = "values",
                     elev_min, elev_max)

  # Define colors and labels. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette using names from colors().
  cols <- c("Flood Prone"   = "coral3",
            "Bankfull"      = "darkslategray4",
            "Water Surface" = "cadetblue3")

  # Draw the graph
  p <- ggplot(data = xs_dims,
              aes(x = km_to_mouth, y = elevations, color = water_levels)) +
  geom_line(size = 2) +
  scale_color_manual(values = cols) +
  scale_x_reverse() +
  theme_bw() +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha('white', 0.6)),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
  labs(title = unique(reach_xs_dims$ReachName),
       x     = "Kilometers",
       y     = "Elevation (NAVD88 feet)")

  # Draw cross section labels
  xs_line <- geom_line(inherit.aes = FALSE,
                       data = xs_lines,
                       aes(x = km_to_mouth, y = values, group = Seq),
                       show.legend = FALSE)
  xs_labels <- geom_text_repel(inherit.aes = FALSE,
                               data = xs_lines[xs_lines$elevations == "elev_max",],
                               aes(x = km_to_mouth, y = values, label = Seq),
                               size = 1.8)

  # Label river features
  if(!is.null(features)) {
     features <- geom_text_repel(inherit.aes = FALSE,
                                 data = features,
                                 aes(x = km_to_mouth,
                                     y = rep(plot_min_y - 0, length(Name)),
                                     label = Name),
                                 nudge_x = 0, angle = 90, size = 1.8,
                                 force = 0.01,
                                 segment.size = 0)
  }

  # Return the plot
  if(label_xs == FALSE &  is.null(features)) return(p)
  if(label_xs == FALSE & !is.null(features)) return(p + features)
  if(label_xs == TRUE  &  is.null(features)) return(p + xs_line + xs_labels)
  if(label_xs == TRUE  & !is.null(features)) return(p + xs_line + xs_labels + features)
  if(label_xs == TRUE  & !is.null(features)) return(p + xs_line + xs_labels + features)
}
