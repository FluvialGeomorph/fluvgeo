#' @title Plot a reach regional hydraulic geometry graph
#'
#' @description Produces a regional hydraulic geometry graph for the specified
#' streams.
#'
#' @export
#' @param xs_dims      data frame; a data frame of cross section dimensions
#'                     generated using `fluvgeo::xs_dimensions`.
#' @param streams      character vector; The stream names in the study area.
#'                     If a single value, then only that stream will be
#'                     displayed. If a vector of stream names is specified
#'                     all streams in the vector will be displayed.
#' @param bf_elevation numeric vector; The bankfull elevations (units:
#'                     detrended feet) that are used to calculate hydraulic
#'                     geometry.
#' @param xs_trend     logical; Smooth trend line?
#' @param log_scale    logical; Convert scales to log?
#'
#' @return A ggplot object.
#'
#' @examples
#' # Extract attribute data from the fluvgeo::sin_xs_points sf
#' sin_xs_points_df <- fluvgeo::sin_riffle_channel_points_sf
#'
#' # Set variable values
#' streams <- "Sinsinawa"
#' regions <- c("Illinois River", "IN Central Till Plain")
#' bankfull_elevations <- seq(103, 104, 0.1)
#'
#' # Call the xs_dimensions function
#' sin <- xs_dimensions(xs_points = sin_xs_points_df,
#'                      streams = streams,
#'                      regions = regions,
#'                      bankfull_elevations = bankfull_elevations)
#'
#' # Call the reach_rhg_graph function
#' sin_reach_rhg_graph <- reach_rhg_graph(xs_dims = sin,
#'                                        streams = streams,
#'                                        bf_elevation = bankfull_elevations)
#'
#' # Print the reach_rhg_graph
#' print(sin_reach_rhg_graph)
#'
#' @importFrom assertthat assert_that
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_string scale_color_brewer facet_grid
#' as_labeller theme_bw theme labs geom_point geom_smooth scale_y_log10
#' scale_x_log10
#' @importFrom ggrepel geom_text_repel
#'
reach_rhg_graph <- function(xs_dims, streams, bf_elevation, xs_trend = FALSE,
                            log_scale = FALSE) {
  # Check parameters
  assert_that(is.data.frame(xs_dims),
              msg = "'xs_dims' must be a data frame")
  assert_that("reach_name" %in% colnames(xs_dims),
              msg = "Required field 'reach_name' is missing from 'xs_dims'")
  assert_that("cross_section" %in% colnames(xs_dims),
              msg = "Required field 'cross_section' is missing from 'xs_dims'")
  assert_that("xs_type" %in% colnames(xs_dims),
              msg = "Required field 'xs_type' is missing from 'xs_dims'")
  assert_that("bankfull_elevation" %in% colnames(xs_dims),
              msg = "Required field 'bankfull_elevation' is missing from
                    'xs_dims'")
  assert_that("drainage_area" %in% colnames(xs_dims),
              msg = "Required field 'drainage_area' is missing from 'xs_dims'")
  assert_that("xs_area" %in% colnames(xs_dims),
              msg = "Required field 'xs_area' is missing from 'xs_dims'")
  assert_that("xs_width" %in% colnames(xs_dims),
              msg = "Required field 'xs_width' is missing from 'xs_dims'")
  assert_that("xs_depth" %in% colnames(xs_dims),
              msg = "Required field 'xs_depth' is missing from 'xs_dims'")
  assert_that(is.character(streams) && length(streams) > 0,
              msg = "streams must be a character vector with at least one
              item")
  assert_that(is.numeric(bf_elevation) && length(bf_elevation) > 0,
              msg = "bankfull_elevations must be a numeric vector with at
              least one item")
  assert_that(is.logical(xs_trend) && length(xs_trend) == 1,
              msg = "'xs_trend' must be boolean of length one")
  assert_that(is.logical(log_scale) && length(log_scale) == 1,
              msg = "'log_scale' must be boolean of length one")

  # Create a data frame to hold only the specified stream(s) and the respective
  # bankfull elevation(s).
  xs_dims_graph <- data.frame()
  # Iterate through stream(s)
  for (i in seq_len(length(streams))) {
    # Filter to select only the current stream.
    xs_dims_stream <- xs_dims[xs_dims$reach_name == streams[i], ]
    # Filter by the respective bankfull elevation for the current stream.
    xs_dims_stream_bankfull <- xs_dims_stream[
                                      xs_dims_stream$bankfull_elevation ==
                                      bf_elevation[i], ]
    # Append the current stream and bankfull records to the running total
    xs_dims_graph <- rbind(xs_dims_graph, xs_dims_stream_bankfull)
  }
  # Subset xs_dims_graph to select regional curve data (no DEM derived cross
  # sections) at the specified bankfull_elevation
  xsd_regions <- xs_dims_graph[xs_dims_graph$xs_type !=
                               "DEM derived cross section", ]
  # Subset xs_dims_stream to selct only DEM derived cross sections at the
  # specified bankfull_elevation
  xsd_dem <- xs_dims_graph[xs_dims_graph$xs_type ==
                           "DEM derived cross section", ]
  # Gather xs dimensions into key-value fields (convert wide to long format)
  # for graphing
  xsd_regions_gather <- melt(data = xsd_regions,
                             id.vars = c("reach_name", "cross_section",
                                         "xs_type", "bankfull_elevation",
                                         "drainage_area"),
                             measure.vars = c("xs_area", "xs_width",
                                              "xs_depth"),
                             variable.name = "stats", value.name = "measure")
  xsd_dem_gather     <- melt(data = xsd_dem,
                             id.vars = c("reach_name", "cross_section",
                                         "xs_type", "bankfull_elevation",
                                         "drainage_area"),
                             measure.vars = c("xs_area", "xs_width",
                                              "xs_depth"),
                             variable.name = "stats", value.name = "measure")
  # Control facet names
  stat_names <- c(`xs_area`  = "Area (sq feet)",
                  `xs_width` = "Width (feet)",
                  `xs_depth` = "Depth (feet)")
  # Define the base plot
  p <- ggplot(data = unique(xsd_regions_gather),
              aes_string(x = 'drainage_area',
                         y = 'measure',
                         color = 'xs_type')) +
    scale_color_brewer(palette = "Set1", name = "Regional Curves") +
    facet_grid(rows = stats ~ .,
               scales = "free",
               labeller = as_labeller(stat_names)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "vertical") +
    labs(x = "Drainage Area (square miles)", y = "")
  # Regional curves
  regional_curves <- geom_line(size = 1)
  # Define cross section points
  xs_pts <- geom_point(inherit.aes = FALSE,
                       data = unique(xsd_dem_gather),
                       aes_string(x = 'drainage_area',
                                  y = 'measure'),
                       size = 1,
                       show.legend = FALSE)
  xs_labels <- geom_text_repel(inherit.aes = FALSE,
                               data = unique(xsd_dem_gather),
                               aes_string(x = 'drainage_area',
                                          y = 'measure',
                                          label = 'cross_section'),
                               show.legend = FALSE,
                               size = 2)
  # Draw a smooth line through cross section points
  xs_trend_line <- geom_smooth(inherit.aes = FALSE,
                               data = unique(xsd_dem_gather),
                               aes_string(x = 'drainage_area',
                                         y = 'measure'))
  # Transform to log scales
  log_scale_y <- scale_y_log10()
  log_scale_x <- scale_x_log10()
  # Define title
  title <- labs(title = paste("Detrended Bankfull Elevation:",
                              bf_elevation, "(feet)"))
  # If only one stream, display cross section points, labels, and title
  if (length(bf_elevation) == 1) {
    return(p + regional_curves + xs_pts + xs_labels + title)
  }
  # If more than one stream, display cross section points
  if (length(bf_elevation) > 1 & xs_trend == FALSE) {
    return(p + regional_curves + xs_pts)
  }
  if (length(bf_elevation) > 1 & xs_trend == TRUE & log_scale == FALSE) {
    return(p + regional_curves + xs_pts + xs_trend_line)
  }
  if (length(bf_elevation) > 1 & xs_trend == TRUE & log_scale == TRUE) {
    return(p + regional_curves + xs_pts + xs_trend_line + log_scale_y +
             log_scale_x)
  }
}

