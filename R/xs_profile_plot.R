#' @title Plot stream longitudinal profile
#'
#' @description Produces a longitudinal profile plot for the cross sections
#' of the input stream reach.
#'
#' @export
#' @param reach_xs_dims_sp SpatialPointsDataFrame of cross section
#'                         dimensions.
#' @param features_sp      SpatialPointsDataFrame of infrastructure features
#' @param label_xs         logical; Draw the cross section labels?
#' @param xs_label_freq    numeric; An integer indicating the frequency of
#'                         cross section labels.
#' @param profile_units    character; the units of the longitudinal profile.
#'                         One of "kilometers", "meters", "miles", or "feet"
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{profile_plot} function requires a \code{xs_dimensions}
#' dataframe. See the \code{sin_xs_dimensions} package dataset for an
#' example of this format of cross section data produced by the
#' \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @examples
#' # Create cross section profile plot
#' profile_plot <- xs_profile_plot(reach_xs_dims_sp = fluvgeo::sin_riffle_floodplain_dims_L3_sp,
#'                                 features_sp = fluvgeo::sin_features_sp,
#'                                 label_xs = TRUE)
#'
#' # Print the plot
#' print(profile_plot)
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw alpha theme element_rect element_blank element_line labs
#'
#'
xs_profile_plot <- function(reach_xs_dims_sp,
                            features_sp = NULL,
                            label_xs = TRUE,
                            xs_label_freq = 10,
                            profile_units = "kilometers") {
  # Check parameters
  check_cross_section_dimensions(reach_xs_dims_sp, "cross_section_dimensions")
  check_features(features_sp)
  assert_that(is.logical(label_xs),
              msg = "label_xs must be logical")
  assert_that(profile_units %in% c("kilometers", "meters", "miles", "feet"),
              msg = 'profile units must be one of "kilometers", "meters",
                    "miles", "feet"')

  # Calculate a unit conversion coefficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Convert to data frames for ggplot
  reach_xs_dims <- reach_xs_dims_sp@data
  features      <- features_sp@data

  # Gather data by water levels for plotting
  xs_dims <- gather(reach_xs_dims,
                    key = "water_levels",
                    value = "elevations",
                    .data$bankfull_elev,
                    .data$watersurface_elev)

  # Determine min y value
  plot_min_y <- min(xs_dims$elevations)

  # Set factor levels to control legend
  xs_dims$water_levels <- factor(xs_dims$water_levels,
                                 levels = c("bankfull_elev",
                                            "watersurface_elev"),
                                 labels = c("Bankfull",
                                            "Water Surface"))

  # Create xs graphing data
  reach_xs_dims$elev_min <- reach_xs_dims$watersurface_elev - 2
  reach_xs_dims$elev_max <- reach_xs_dims$bankfull_elev + 2
  xs_lines <- tidyr::gather(reach_xs_dims,
                            key = "elevations",
                            value = "values",
                            .data$elev_min, .data$elev_max)

  # Determine cross section label frequency
  labeled_xs <- ((xs_lines$Seq + xs_label_freq) %% xs_label_freq) == 0
  xs_labels_sf <- xs_lines[labeled_xs, ]

  # Calculate y-axis minor breaks interval
  ymin <- floor(min(reach_xs_dims$elev_min))
  ymax <- ceiling(max(reach_xs_dims$elev_max))

  # Define colors and labels. Inspired by palettes from
  # https://www.tumblr.com/search/wes%20anderson%20palette using names
  # from colors().
  cols <- c("Bankfull"      = "darkslategray4",
            "Water Surface" = "cadetblue3")

  # Draw the graph
  p <- ggplot(xs_dims, aes(x = .data$km_to_mouth * unit_coef,
                           y = .data$elevations,
                           color = .data$water_levels)) +
  geom_line(size = 2) +
  scale_color_manual(values = cols) +
  scale_y_continuous(minor_breaks = seq(ymin, ymax, 1)) +
  theme_bw() +
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = alpha('white', 0.6)),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1)) +
  labs(title = unique(reach_xs_dims$ReachName),
       x     = profile_units,
       y     = "Elevation (NAVD88 feet)")

  # Draw cross section labels
  xs_line <- geom_line(inherit.aes = FALSE,
                       data = xs_lines,
                       aes(x = .data$km_to_mouth * unit_coef,
                           y = .data$values,
                           group = .data$Seq),
                       show.legend = FALSE)
  xs_labels <- geom_text_repel(inherit.aes = FALSE,
                               data = xs_labels_sf[xs_labels_sf$elevations == "elev_max",],
                               aes(x = .data$km_to_mouth * unit_coef,
                                   y = .data$values,
                                   label = .data$Seq),
                               size = 3)

  # Label river features
  if(!is.null(features)) {
     features <- geom_text_repel(inherit.aes = FALSE,
                                 data = features,
                                 aes(x = .data$km_to_mouth * unit_coef,
                                     y = rep(plot_min_y - 0, length(.data$Name)),
                                     label = .data$Name),
                                 nudge_x = 0, angle = 90, size = 3,
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
