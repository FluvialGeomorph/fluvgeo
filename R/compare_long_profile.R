#' @title Compare stream longitudinal profiles from multiple surveys
#'
#' @description Compare stream longitudinal profiles from multiple surveys.
#'
#' @export
#' @param stream               character; The name of the stream.
#' @param flowline_pts_sf_list list; a list of flowline_points
#'                             `sf` objects`, one for each survey time
#'                             period to be graphed. Survey list items must be
#'                             tagged with the survey label to be used in the
#'                             graph legend.
#' @param features_sf          `sf` data frame of infrastructure features.
#' @param profile_units        character; the units of the longitudinal profile.
#'                             One of "kilometers", "meters", "miles", or "feet"
#'
#' @return A ggplot2 object.
#'
#' @seealso The \code{compare_long_profile} function requires a
#' \code{flowline_points} `sf` object. See the
#' \code{sin_flowline_points_sf} package dataset for an example of this format
#' of cross section data produced by the \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom rlang .data
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw alpha theme element_rect element_blank element_line labs
#'
#'
compare_long_profile <- function(stream, flowline_pts_sf_list,
                                 features_sf = NULL, profile_units = "feet") {
  # Extract data frames (for ggplot2) from the sf objects
  flowline_pts_df <- purrr::map(flowline_pts_sf_list,
                                sf::st_drop_geometry)

  # Convert features_sf to data frame for ggplot2
  features <- sf::st_drop_geometry(features_sf)

  # Filter for the current reach
  flowline_current <- purrr::map(flowline_pts_df,
                                 ~dplyr::filter(.x, ReachName == stream))

  # Combine surveys
  flowline_pts <- dplyr::bind_rows(flowline_current, .id = "Survey")

  # Define survey factor levels
  survey_levels <- sort(unique(as.character(flowline_pts$Survey)),
                        decreasing = TRUE)
  flowline_pts$Survey <- factor(flowline_pts$Survey,
                                levels = survey_levels,
                                labels = survey_levels,
                                ordered = TRUE)

  # Determine min y value
  plot_min_y <- min(flowline_pts$Z)

  # Define colors
  cols <- c("coral3", "darkslategray4", "darkolivegreen", "mediumpurple4")

  # Calculate a unit conversion coefficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Draw the graph
  p <- ggplot(flowline_pts,
              aes(x = .data$POINT_M * unit_coef,
                  y = .data$Z,
                  color = .data$Survey)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = cols) +
  #scale_x_reverse() +
  theme_bw() +
  theme(legend.position = c(.1, .9),
        legend.background = element_rect(fill = alpha('white', 0.6)),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
  labs(title = unique(flowline_pts$ReachName),
       x     = profile_units,
       y     = "Elevation (NAVD88 feet)")

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
  if(is.null(features)) return(p)
  if(!is.null(features)) return(p + features)
}
