#' @title Calculate slope and sinuosity
#'
#' @description Calculates the slope and sinuosity of the input channel
#' features.
#'
#' @export
#' @param channel_features Spatial*DataFrame; a `fluvgeo` data structure of
#'                         channel features (i.e., cross section, flowline
#'                         points, etc.) Must have the following fields:
#'                         `ReachName`, `POINT_X`, `POINT_Y`, `POINT_M`, `Z`
#' @param lead_n           numeric; The number of features to lead (upstream)
#'                         to calculate the slope and sinuosity. Must be an
#'                         integer.
#' @param lag_n            numeric; The number of features to lag (downstream)
#'                         to calculate the slope and sinuosity. Must be an
#'                         integer.
#' @param use_smoothing    boolean; determines if smoothed elevation values
#'                         are used to calculate gradient. values are:
#'                         TRUE, FALSE (default)
#' @param loess_span       numeric; the loess regression span parameter,
#'                         defaults to 0.05
#' @param vert_units       character; The vertical units. One of: "m" (meter),
#'                         "ft" (foot), "us-ft" (us survey foot)
#'
#' @return A dataframe of slope and sinuosity dimensions representing the
#' position of each feature within the channel.
#'
#' @examples
#' # Call the slope_sinuosity function for a flowline
#' sin_flowline_ss <- slope_sinuosity(fluvgeo::sin_flowline_points_sp,
#'                                    lead_n = 100, lag_n = 0,
#'                                    vert_units = "ft")
#'
#' # Call the slope_sinuosity function for a cross section
#' sin_riffle_channel_ss <- slope_sinuosity(fluvgeo::sin_riffle_channel_sp,
#'                                          lead_n = 3, lag_n = 0,
#'                                          loess_span = 0.5,
#'                                          vert_units = "ft")
#'
#' @importFrom assertthat assert_that
#' @importFrom stats loess predict
#' @importFrom dplyr first last lead lag
#' @importFrom raster pointDistance
#' @importFrom sp proj4string
#'
slope_sinuosity <-function(channel_features, lead_n, lag_n,
                           use_smoothing = TRUE,
                           loess_span = 0.5,
                           vert_units) {
  name <- deparse(substitute(channel_features))

  # Check data structure
  assert_that(is.data.frame(channel_features@data),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(channel_features@data) &
                is.character(channel_features@data$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("POINT_X" %in% colnames(channel_features@data) &
                is.numeric(channel_features@data$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(channel_features@data) &
                is.numeric(channel_features@data$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(channel_features@data) &
                is.numeric(channel_features@data$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))
  assert_that("Z" %in% colnames(channel_features@data) &
                is.numeric(channel_features@data$Z),
              msg = paste("Numeric field 'Z' missing from ", name))

  # Check parameters
  assert_that(as.integer(lead_n) == lead_n &&
                length(lead_n) == 1,
              msg = "'lead_n' must be an integer vector of length one")
  assert_that(as.integer(lag_n) == lag_n &&
                length(lag_n) == 1,
              msg = "'lag_n' must be an integer vector of length one")
  assert_that(is.logical(use_smoothing) &&
                length(use_smoothing) == 1,
              msg = "'use_smoothing' must be logical vector of length one")
  assert_that(is.numeric(loess_span) &&
                length(loess_span) == 1,
              msg = "'loess_span' must be numeric vector of length one")

  # Notes on FluvialGeomorph units:
  # POINT_M - These values will always be in kilometers and therefore need
  # to be converted into feet: 1 km = 3280.48 ft.
  # Z - FluvialGeomorph Z vertical units are by convention in feet, but
  # controlled for in this function by the `vert_con_factor`.
  # POINT_X, POINT_Y - X and Y horizontal units are in the units of the
  # channel_features' coordinate system. Distances calculated using them are
  # converted to feet in this function by the `horiz_con_factor`.

  # Set the horizontal unit conversion factor to calculate feet
  if(any(grep("units=m", sp::proj4string(channel_features))) == 1) {
    horiz_con_factor <- 3.28084}
  if(any(grep("units=ft", sp::proj4string(channel_features))) == 1) {
    horiz_con_factor <- 1}
  if(any(grep("units=us-ft", sp::proj4string(channel_features))) == 1) {
    horiz_con_factor <- 0.999998000004}

  # Set the vertical unit conversion factor to calculate feet
  if(vert_units == "m") vert_con_factor <- 3.28084
  if(vert_units == "ft") vert_con_factor <- 1
  if(vert_units == "us-ft") vert_con_factor <- 0.999998000004

  # Convert Spatial*DataFrame to a data frame
  channel_features <- channel_features@data

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
  channel_features$sinuosity_nlt_one <- 0
  channel_features$slope             <- 0
  channel_features$slope_gte_zero    <- 0

  # Sort by ReachName and POINT_M
  flowline_pts <- channel_features[order(channel_features$ReachName,
                                         channel_features$POINT_M), ]

  # Iterate through reaches and calculate gradient and sinuosity
  reaches <- levels(as.factor(flowline_pts$ReachName))
  for (r in reaches) {
    # Subset flowline_pts for the current reach
    fl_pts <- flowline_pts[flowline_pts$ReachName == r, ]

    # Calculate a smoothed z (and convert to feet).
    smooth_z <- loess(Z ~ POINT_M, data = fl_pts, span = loess_span)
    fl_pts$Z_smooth <- predict(smooth_z) * vert_con_factor

    # Calculate variable mins and maxs (ensure units in feet).
    # Used as default to lead/lag to prevent NAs being introduced at ends
    # of series.
    upstream_m_lead         <- max(fl_pts$POINT_M) * 3280.48
    downstream_m_lag        <- min(fl_pts$POINT_M) * 3280.48
    upstream_z_smooth_lead  <- max(fl_pts$Z_smooth)
    downstream_z_smooth_lag <- min(fl_pts$Z_smooth)
    upstream_z_lead         <- max(fl_pts$Z) * vert_con_factor
    downstream_z_lag        <- min(fl_pts$Z) * vert_con_factor

    # Calculate upstream/downstream z values (ensure units in feet)
    if (use_smoothing == TRUE) {
      fl_pts$upstream_z   <- lead(x = fl_pts$Z_smooth,
                                  n = lead_n,
                                  default = upstream_z_smooth_lead)
      fl_pts$downstream_z <- lag(x = fl_pts$Z_smooth,
                                 n = lag_n,
                                 default = downstream_z_smooth_lag)
    }
    if (use_smoothing == FALSE) {
      fl_pts$upstream_z   <- lead(x = fl_pts$Z * vert_con_factor,
                                  n = lead_n,
                                  default = upstream_z_lead)
      fl_pts$downstream_z <- lag(x = fl_pts$Z * vert_con_factor,
                                 n = lag_n,
                                 default = downstream_z_lag)
    }

    # Calculate upstream/downstream m-values (ensure units in feet).
    fl_pts$upstream_m   <- lead(x = fl_pts$POINT_M * 3280.48,
                                n = lead_n,
                                default = upstream_m_lead)
    fl_pts$downstream_m <- lag(x = fl_pts$POINT_M * 3280.48,
                               n = lag_n,
                               default = downstream_m_lag)

    # Calculate rise and run (already in feet)
    fl_pts$rise <- fl_pts$upstream_z - fl_pts$downstream_z
    fl_pts$run  <- fl_pts$upstream_m - fl_pts$downstream_m

    # Calculate slope: (rise / run)
    fl_pts$slope <- fl_pts$rise / fl_pts$run

    # Calculate slope greater than or equal to zero
    fl_pts$slope_gte_zero <- fl_pts$slope
    fl_pts$slope_gte_zero[fl_pts$slope_gte_zero < 0] <- 0

    # Calculate coords of first and last record. Use as default to lead/lag
    # to prevent NAs being introduced at ends of series.
    upstream_x_lead  <- last(fl_pts$POINT_X)
    downstream_x_lag <- first(fl_pts$POINT_X)
    upstream_y_lead  <- last(fl_pts$POINT_Y)
    downstream_y_lag <- first(fl_pts$POINT_Y)

    # Calculate x values (units in original coordinate system units)
    fl_pts$upstream_x   <- lead(x = fl_pts$POINT_X,
                                n = lead_n,
                                default = upstream_x_lead)
    fl_pts$downstream_x <- lag(x = fl_pts$POINT_X,
                               n = lag_n,
                               default = downstream_x_lag)

    # Calculate y values (units in original coordinate system units)
    fl_pts$upstream_y   <- lead(x = fl_pts$POINT_Y,
                                n = lead_n,
                                default = upstream_y_lead)
    fl_pts$downstream_y <- lag(x = fl_pts$POINT_Y,
                               n = lag_n,
                               default = downstream_y_lag)

    # Calculate stream_length (already in feet)
    fl_pts$stream_length <- fl_pts$upstream_m - fl_pts$downstream_m

    # Calculate valley_length (and convert horiz. units to feet)
    fl_pts$valley_length <- pointDistance(p1 = cbind(fl_pts$upstream_x,
                                                     fl_pts$upstream_y),
                                          p2 = cbind(fl_pts$downstream_x,
                                                     fl_pts$downstream_y),
                                          lonlat = FALSE) * horiz_con_factor

    # Calculate sinuosity: (stream_length / valley_length)
    fl_pts$sinuosity <- fl_pts$stream_length / fl_pts$valley_length

    # Calculate sinuosity not less than one
    fl_pts$sinuosity_nlt_one <- fl_pts$sinuosity
    fl_pts$sinuosity_nlt_one[fl_pts$sinuosity_nlt_one < 1] <- 1

    return(fl_pts)
  }
}
