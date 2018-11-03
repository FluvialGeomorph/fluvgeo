#' @title Plot goodness-of-fit graph
#'
#' @description Produces a goodness-of-fit statistic graph for the specified
#' reach for analysis regions specified by the \code{\link{build_gof_stats}}
#' function.
#'
#' @export
#' @param gof_stats           data frame; A gof_stats data frame. See the
#'                            \code{\link{build_gof_stats}} function for
#'                            details on how to create.
#' @param stream              character; The name of the stream.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#' @param stat                character; The statistic to graph "RMSE", "MAE"
#'                            (the default).
#'
#' @return A ggplot object.
#'
#' @details Add a discussion of how to interpret the graph.
#'
#' @seealso The \code{\link{build_gof_stats}} function for details on how to
#' create a gof_stats data frame.
#'
#' @examples
#' # Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
#' sin_xs_points_df <- fgm::sin_xs_points@@data
#'
#' # Set variable values
#' streams <- c("Sinsinawa")
#' regions <- c("Eastern United States", "IN Central Till Plain")
#' bankfull_elevations = seq(103, 104, 0.1)
#'
#' # Call the xs_dimensions function
#' sin <- xs_dimensions(xs_points = sin_xs_points_df,
#'                      streams = streams,
#'                      regions = regions,
#'                      bankfull_elevations = bankfull_elevations)
#'
#' # Call the build_gof_stats function
#' sin_gof <- build_gof_stats(xs_dims = sin,
#'                            streams = streams,
#'                            regions = regions,
#'                            bankfull_elevations = bankfull_elevations)
#'
#' # Call the gof_graph function
#' sin_gof_graph <- gof_graph(gof_stats = sin_gof,
#'                            stream = streams,
#'                            bankfull_elevation = 103.5,
#'                            stat = "MAE")
#'
#' # Print the graph
#' sin_gof_graph
#'
#' @importFrom assertthat assert_that
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_string geom_line facet_grid as_labeller
#' theme_bw geom_vline scale_color_brewer theme unit element_text labs
#'
gof_graph <- function(gof_stats, stream, bankfull_elevation,
                      stat = "MAE") {
  # Check parameters
  assert_that(is.data.frame(gof_stats),
              msg = "'gof_stats' must be a data frame")
  assert_that("reach_name" %in% colnames(gof_stats),
              msg = "Required field 'reach_name' is missing from 'gof_stats'")
  assert_that("region" %in% colnames(gof_stats),
              msg = "Required field 'region' is missing from 'gof_stats'")
  assert_that("bankfull_elevation" %in% colnames(gof_stats),
              msg = "Required field 'bankfull_elevation' is missing from
                    'gof_stats'")
  assert_that("rmse_area" %in% colnames(gof_stats),
              msg = "Required field 'rmse_area is missing from 'gof_stats'")
  assert_that("rmse_width" %in% colnames(gof_stats),
              msg = "Required field 'remse_width' is missing from 'gof_stats'")
  assert_that("rmse_depth" %in% colnames(gof_stats),
              msg = "Required field 'rmse_depth' is missing from 'gof_stats'")
  assert_that("mae_area" %in% colnames(gof_stats),
              msg = "Required field 'mae_area' is missing from 'gof_stats'")
  assert_that("mae_width" %in% colnames(gof_stats),
              msg = "Required field 'mae_width' is missing from 'gof_stats'")
  assert_that("mae_depth" %in% colnames(gof_stats),
              msg = "Required field 'mae_depth' is missing from 'gof_stats'")
  assert_that(is.character(stream) && nchar(stream) != 0 &&
                length(stream) == 1,
              msg = "stream must be a character vector of length one")
  assert_that(is.numeric(bankfull_elevation) &&
                length(bankfull_elevation) == 1,
              msg = "bankfull_elevation must be a numeric vector of
              length one")
  assert_that(is.character(stat) && (stat == "MAE" | stat == "RMSE") &&
              length(stat) == 1,
              msg = "stat must be a character vector of length one containing
                     the value 'MAE' or 'RMSE'" )

  # Gather the gof_stats into key-value fields (convert wide to long format)
  # use melt as a SE replacement for gather which is NSE
  gof_stats_gather <- melt(data = gof_stats,
                           id.vars = c("reach_name", "region",
                                       "bankfull_elevation"),
                           measure.vars = c("rmse_area", "rmse_width",
                                            "rmse_depth", "mae_area",
                                            "mae_width", "mae_depth"),
                           variable.name = "stats", value.name = "measure")

  # Subset gof_stats_gather to include only the specified stream
  gof_stats_stream <- gof_stats_gather[gof_stats_gather$reach_name == stream, ]

  # Use the stat parameter to select which statistics to graph
  if(stat == "MAE") {
    # Subset gof_stats_stream to select MAE stats
    gsg <- gof_stats_stream[gof_stats_stream$stats == "mae_area" |
                            gof_stats_stream$stats == "mae_width" |
                            gof_stats_stream$stats == "mae_depth", ]
    # Control facet names
    stat_names <- c(`mae_area`  = "Area (sq feet)",
                    `mae_width` = "Width (feet)",
                    `mae_depth` = "Depth (feet)")
  }
  if(stat == "RMSE") {
    # Subset gof_stats_stream to select RMSE stats
    gsg <- gof_stats_stream[gof_stats_stream$stats == "rmse_area" |
                            gof_stats_stream$stats == "rmse_width" |
                            gof_stats_stream$stats == "rmse_depth", ]
    # Control facet names
    stat_names <- c(`rmse_area`  = "Area (sq feet)",
                    `rmse_width` = "Width (feet)",
                    `rmse_depth` = "Depth (feet)")
  }
  # Draw the plot
  q <- ggplot(data = gsg,
              aes_string(x = 'bankfull_elevation',
                         y = 'measure',
                         color = 'region')) +
    geom_line(size = 0.75) +
    facet_grid(facets = stats ~ .,
               scales = "free",
               labeller = as_labeller(stat_names)) +
    theme_bw() +
    geom_vline(xintercept = bankfull_elevation, colour = "black") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.direction = "vertical",
          legend.position = c(0.15, 0.9),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 9)
    ) +
    labs(x = "Detrended Bankfull Elevation (feet)",
         y = stat)
  return(q)
}
