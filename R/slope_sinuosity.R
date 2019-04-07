#' @title Calculate slope and sinuosity
#'
#' @description Calculates the slope and sinuosity of the input channel
#' features.
#'
#' @export
#' @param channel_features data frame; a data frame of channel features (i.e.,
#'                         cross section, flowline points, etc.)
#' @param lead_lag         numeric; The number of features to lead/lag on
#'                         either side of each feature that will be used to
#'                         calculate the slope and sinuosity. Must be an
#'                         integer.
#' @param use_smoothing    boolean; determines if smoothed elevation values
#'                         are used to calculate gradient. values are:
#'                         TRUE, FALSE (default)
#' @param loess_span       numeric; the loess regression span parameter,
#'                         defaults to 0.05
#'
#' @return A dataframe of dimensions representing the position of each feature
#' within the channel.
#'
#' @examples
#' # Extract attribute data from the fgm::sin_flowline_points SpatialPointsDataFrame
#' sin_flowline_points_df <- fgm::sin_flowline_points@data
#'
#' # Extract data from the fgm::sin_riffle SpatialLinesDataFrame
#' sin_riffle_df <- fgm::sin_riffle@data
#'
#' # Call the slope_sinuosity function for a flowline
#' sin_flowline_ss <- slope_sinuosity(sin_flowline_points_df,
#'                                    lead_lag = 1000)
#'
#' # Call the slope_sinuosity function for a cross section
#' sin_riffle_ss <- slope_sinuosity(sin_riffle_df,
#'                                  lead_lag = 1,
#'                                  loess_span = 5)
#'
#' @importFrom assertthat assert_that
#' @importFrom stats loess predict
#' @importFrom dplyr first last lead lag
#' @importFrom raster pointDistance
#'
slope_sinuosity <-function(channel_features,
                           lead_lag,
                           use_smoothing = TRUE,
                           loess_span = 0.05) {
  # Check parameters
  assert_that(check_data_structure(channel_features, "channel_feature"),
              msg = "'channel_features' does not meet the data specification")
  assert_that(as.integer(lead_lag) == lead_lag &&
                length(lead_lag) == 1,
              msg = "'lead_lag' must be an integer vector of length one")
  assert_that(is.logical(use_smoothing) &&
                length(use_smoothing) == 1,
              msg = "'use_smoothing' must be logical vector of length one")
  assert_that(is.numeric(loess_span) &&
                length(loess_span) == 1,
              msg = "'loess_span' must be numeric vector of length one")

  # Add new columns to hold calculated values
  channel_features$Z_smooth      <- 0
  channel_features$upstream_x    <- 0
  channel_features$upstream_y    <- 0
  channel_features$downstream_x  <- 0
  channel_features$downstream_y  <- 0
  channel_features$upstream_z    <- 0
  channel_features$downstream_z  <- 0
  channel_features$upstream_m    <- 0
  channel_features$downstream_m  <- 0
  channel_features$rise          <- 0
  channel_features$run           <- 0
  channel_features$stream_length <- 0
  channel_features$valley_length <- 0
  channel_features$sinuosity     <- 0
  channel_features$slope         <- 0

  # Sort by ReachName and POINT_M
  flowline_pts <- channel_features[order(channel_features$ReachName,
                               channel_features$POINT_M),]

  # Iterate through reaches and calculate gradient and sinuosity
  reaches <- levels(as.factor(flowline_pts$ReachName))
  for (r in reaches) {
    print(r)
    # Subset flowline_pts for the current reach
    fl_pts <- flowline_pts[flowline_pts$ReachName == r, ]

    ## Calculate gradient-slope
    # Calculate a loess smoothed z
    l_z_5 <- loess(Z ~ POINT_M, data = fl_pts, span = loess_span)
    fl_pts$Z_smooth <- predict(l_z_5)

    # Calculate variable mins and maxs. Use as default to lead/lag to
    # prevent NAs being introduced at ends of series.
    upstream_m_lead         <- max(fl_pts$POINT_M)
    downstream_m_lag        <- min(fl_pts$POINT_M)
    upstream_z_smooth_lead  <- max(fl_pts$Z_smooth)
    downstream_z_smooth_lag <- min(fl_pts$Z_smooth)
    upstream_z_lead         <- max(fl_pts$Z)
    downstream_z_lag        <- min(fl_pts$Z)

    # Calculate z values (already in feet)
    if (use_smoothing == TRUE) {
      fl_pts$upstream_z   <- lead(x = fl_pts$Z_smooth,
                                  n = lead_lag,
                                  default = upstream_z_smooth_lead)
      fl_pts$downstream_z <- lag(x = fl_pts$Z_smooth,
                                 n = lead_lag,
                                 default = downstream_z_smooth_lag)
    }
    if (use_smoothing == FALSE) {
      fl_pts$upstream_z   <- lead(x = fl_pts$Z,
                                  n = lead_lag,
                                  default = upstream_z_lead)
      fl_pts$downstream_z <- lag(x = fl_pts$Z,
                                 n = lead_lag,
                                 default = downstream_z_lag)
    }

    # Calculate m values (and convert from kilometers to feet: 1 km =
    # 3280.84 ft)
    fl_pts$upstream_m   <- lead(x = fl_pts$POINT_M,
                                n = lead_lag,
                                default = upstream_m_lead) * 3280.48
    fl_pts$downstream_m <- lag(x = fl_pts$POINT_M,
                               n = lead_lag,
                               default = downstream_m_lag) * 3280.48

    # Calculate rise and run (in feet)
    fl_pts$rise <- fl_pts$upstream_z - fl_pts$downstream_z
    fl_pts$run  <- fl_pts$upstream_m - fl_pts$downstream_m

    # Calculate slope: (rise / run)
    fl_pts$slope <- fl_pts$rise / fl_pts$run

    ## Calculate sinuosity
    # Calculate coords of first and last record. Use as default to lead/lag
    # to prevent NAs being introduced at ends of series.
    upstream_x_lead  <- last(fl_pts$POINT_X)
    downstream_x_lag <- first(fl_pts$POINT_X)
    upstream_y_lead  <- last(fl_pts$POINT_Y)
    downstream_y_lag <- first(fl_pts$POINT_Y)

    # Calculate x values
    fl_pts$upstream_x   <- lead(x = fl_pts$POINT_X,
                                n = lead_lag,
                                default = upstream_x_lead)
    fl_pts$downstream_x <- lag(x = fl_pts$POINT_X,
                               n = lead_lag,
                               default = downstream_x_lag)

    # Calculate y values
    fl_pts$upstream_y   <- lead(x = fl_pts$POINT_Y,
                                n = lead_lag,
                                default = upstream_y_lead)
    fl_pts$downstream_y <- lag(x = fl_pts$POINT_Y,
                               n = lead_lag,
                               default = downstream_y_lag)

    # Calculate stream_length (in feet)
    fl_pts$stream_length <- fl_pts$upstream_m - fl_pts$downstream_m

    # Calculate valley_length (convert from meters to feet)
    fl_pts$valley_length <- pointDistance(p1 = cbind(fl_pts$upstream_x,
                                                     fl_pts$upstream_y),
                                          p2 = cbind(fl_pts$downstream_x,
                                                     fl_pts$downstream_y),
                                          lonlat = FALSE) * 3.28084

    # Calculate sinuosity stream_length / valley_length
    fl_pts$sinuosity <- fl_pts$stream_length / fl_pts$valley_length

    return(fl_pts)
  }
}
