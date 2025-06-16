#' @title Overplot multiple cross section profile surveys
#'
#' @description Produces a cross section profile plot for the specified cross
#' section for the input surveys.
#'
#' @export
#' @param stream              character; The name of the stream.
#' @param xs_number           integer; The cross section `Seq` number.
#' @param xs_pts_sf_list      list; a list of `sf` objects of cross
#'                            section points, one for each survey time period
#'                            to be graphed. Survey list items must be tagged
#'                            with the survey label to be used in the graph
#'                            legend.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#' @param aspect_ratio        numeric; The aspect ratio of the graph.
#' @param extent              character; The extent of the cross section to
#'                            plot. One of "all", "floodplain", or "channel".
#'
#' @return A ggplot2 object.
#'
#' @details This function is used to plot the cross section profile from a
#' series of \code{xs_points} data frames representing multiple surveys.
#'
#' @seealso The \code{xs_compre_plot} function requires a \code{xs_points}
#' dataframe. See the \code{sin_xs_points} package dataset for an example of
#' this format of cross section data produced by the \code{FluvialGeomorph}
#' ArcGIS toolbox.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom purrr map
#' @importFrom dplyr filter bind_rows
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_line scale_y_continuous sec_axis
#'               scale_color_manual geom_hline theme_bw theme element_rect
#'               element_blank element_line element_text labs
#'
xs_compare_plot_L2 <- function(stream, xs_number, xs_pts_sf_list,
                               bankfull_elevation, aspect_ratio = 0.5,
                               extent = "all") {
  # Check parameters

  # Extract data frames (for ggplot2) from the sf objects
  xs_pts_df <- purrr::map(xs_pts_sf_list, sf::st_drop_geometry)

  # Filter for the current reach, xs_number, extent
  if(extent == "all") {
    xs_current <- purrr::map(xs_pts_df,
                             ~dplyr::filter(.x, ReachName == stream &
                                              Seq == xs_number))
  }
  if(extent == "floodplain") {
    xs_current <- purrr::map(xs_pts_df,
                             ~dplyr::filter(.x, ReachName == stream &
                                              Seq == xs_number &
                                              floodplain == 1))
  }
  if(extent == "channel") {
    xs_current <- purrr::map(xs_pts_df,
                             ~dplyr::filter(.x, ReachName == stream &
                                              Seq == xs_number &
                                              channel == 1))
  }

  # Combine surveys
  xs_pts <- dplyr::bind_rows(xs_current, .id = "Survey")

  # Define survey factor levels
  survey_levels <- sort(unique(as.character(xs_pts$Survey)),
                        decreasing = TRUE)
  xs_pts$Survey <- factor(xs_pts$Survey,
                          levels = survey_levels,
                          labels = survey_levels,
                          ordered = TRUE)

  # Determine the base survey
  base_survey <- names(xs_pts_sf_list)[1]

  # Filter for the base_survey
  xs_pts_base_survey <- dplyr::filter(xs_pts, .data$Survey == base_survey)

  # Calculate the transform between actual elevation and detrend elevation
  transform <- mean(xs_pts_base_survey$Detrend_DEM_Z -
                      xs_pts_base_survey$DEM_Z)

  # Define colors
  cols <- c("coral3", "darkslategray4", "darkolivegreen", "mediumpurple4")

  # Define minor break interval
  minor_breaks <- seq(floor(min(xs_pts$DEM_Z)),
                      ceiling(max(xs_pts$DEM_Z)), by = 1)

  # Draw the graph
  p <- ggplot(data = xs_pts,
              aes(x = .data$POINT_M * 3.28084,
                  y = .data$DEM_Z,
                  color = .data$Survey)) +
    geom_line(size = 1.25) +
    scale_y_continuous(sec.axis = sec_axis(~. + transform,
                                           name = "Relative Elevation (feet)"),
                       minor_breaks = minor_breaks) +
    scale_color_manual(values = cols) +
    geom_hline(yintercept = bankfull_elevation - transform,
               colour = "blue", size = 1) +
    theme_bw() +
    theme(aspect.ratio = aspect_ratio,
          legend.direction = "vertical",
          legend.position = c(0.07,0.2),
          legend.background = element_rect(fill = alpha('white', 0.6)),
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey", size = 0.1),
          plot.title = element_text(hjust = 0,
                                    size = 10,
                                    face = "bold")) +
    labs(title = paste0("Cross Section ",
                        as.character(xs_number),
                        " (", as.character(extent), " stations)"),
         x = "Station Distance (feet, from left descending bank, looking downstream)",
         y = "Elevation (NAVD88 feet)")
  return(p)
}
