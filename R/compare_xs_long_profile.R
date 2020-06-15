#' @title Compare cross section longitudinal profiles from multiple surveys
#'
#' @description Compare cross section longitudinal profiles from multiple
#' surveys.
#'
#' @export
#' @param stream              character; The name of the stream.
#' @param xs_pts_sf_list      list; a list of `sf` objects of cross
#'                            section points, one for each survey time period
#'                            to be graphed. Survey list items must be tagged
#'                            with the survey label to be used in the graph
#'                            legend.
#' @param features_sf         `sf` object` of infrastructure features.
#' @param profile_units       character; the units of the longitudinal profile.
#'                            One of "kilometers", "meters", "miles", or "feet"
#' @param label_xs            logical; Draw the cross section labels?
#'
#' @return A ggplot2 object.
#'
#' @details This function is used to plot the cross section longitudinal
#' profile from a series of \code{xs_points} data frames representing
#' multiple surveys.
#'
#' @seealso This function requires a
#' \code{flowline_points} SpatialPointsDataFrames. See the
#' \code{sin_flowline_points_sp} package dataset for an example of this format
#' of cross section data produced by the \code{FluvialGeomorph} ArcGIS toolbox.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_x_reverse
#' theme_bw alpha theme element_rect element_blank element_line labs
#'
#'
compare_xs_long_profile <- function(stream, xs_pts_sf_list, features_sf = NULL,
                                    profile_units = "feet", label_xs = TRUE) {
  # Check parameters
  #check_features(features_sp)
  assert_that(is.logical(label_xs), msg = "label_xs must be logical")

  # Extract data frames (for ggplot2) from the sp objects
  xs_pts_df <- purrr::map(xs_pts_sf_list, sf::st_drop_geometry)

  # Convert features_sp to data frame for ggplot2
  features <- sf::st_drop_geometry(features_sf)

  # Filter for the current reach
  stream_current <- purrr::map(xs_pts_df, ~filter(.x, ReachName == stream))

  # Combine surveys
  xs_pts <- dplyr::bind_rows(stream_current, .id = "Survey")

  # Define survey factor levels
  xs_pts$Survey <- factor(xs_pts$Survey)

  # Find the lowest elevation for each cross section
  xs_pts_survey_seq_grouped <- group_by(xs_pts, Survey, Seq, km_to_mouth)
  xs_pts_seq <- summarize(xs_pts_survey_seq_grouped, dem_z_min = min(DEM_Z))

  # Create xs graphing data
  xs_lines_grouped <- group_by(xs_pts, Seq, km_to_mouth)
  xs_lines_min_max <- summarize(xs_lines_grouped, line_top    = min(DEM_Z) - 0.25,
                                                  line_bottom = min(DEM_Z) - 1.5)
  xs_lines <- gather(xs_lines_min_max,
                     key = "elevations",
                     value = "values",
                     .data$line_top, .data$line_bottom)

  # Determine min y value
  plot_min_y <- min(xs_pts$DEM_Z)

  # Define colors
  cols <- c("coral3", "darkslategray4", "darkolivegreen", "mediumpurple4")

  # Calculate a unit conversion coeficient from kilometers to other units
  unit_coef <- switch(profile_units,
                      "kilometers" = 1,
                      "meters"     = 1000,
                      "miles"      = 0.621371,
                      "feet"       = 3280.84)

  # Draw the graph
  p <- ggplot(xs_pts_seq,
              aes(x = .data$km_to_mouth * unit_coef,
                  y = .data$dem_z_min,
                  color = .data$Survey)) +
  geom_line(size = 1.0) +
  geom_point(size = 1.25) +
  scale_color_manual(values = cols) +
  #scale_x_reverse() +
  theme_bw() +
  theme(legend.position = c(.1, .9),
        legend.background = element_rect(fill = alpha('white', 0.6)),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey10", size = 0.1)) +
  labs(title = unique(xs_pts$ReachName),
       x     = profile_units,
       y     = "Elevation (NAVD88 feet)")

  # Draw cross section labels
  xs_line <- geom_line(inherit.aes = FALSE,
                       data = xs_lines,
                       aes(x = .data$km_to_mouth * unit_coef,
                           y = .data$values,
                           group = .data$Seq),
                       show.legend = FALSE)
  xs_labels <- geom_text(inherit.aes = FALSE,
                         data = xs_lines[xs_lines$elevations == "line_top",],
                         aes(x = .data$km_to_mouth * unit_coef,
                             y = .data$values - 1.5,
                             label = .data$Seq),
                             size = 1.5)

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
  if(is.null(features)) return(p + xs_line + xs_labels)
  if(!is.null(features)) return(p + xs_line + xs_labels + features)
}
