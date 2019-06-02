#' @title Calculate the meander width for each loop
#'
#' @description Calculates the meander width (channel belt width) for each loop
#' in the input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  data frame; a data frame of bankline points
#'
#' @return Returns a data frame of loops with the calculated meander width.
#'
#' @importFrom assertthat assert_that
#' @importFrom stats aggregate
#' @importFrom dplyr last lead lag
#' @importFrom raster pointDistance
#'
meander_width <- function(bankline_points) {
  # Check parameters
  assert_that(check_data_structure(bankline_points, "channel_feature"),
              msg = "'bankline_points' does not meet the channel_feature data specification")
  assert_that(check_data_structure(bankline_points, "bankline_points"),
              msg = "'bankline_points' does not meet the bankline_points data specification")

  # Select loop apex points
  loop_apex_points <- bankline_points[bankline_points$position == "apex", ]

  # Sort bankline_points by loop, bend, and POINT_M
  apex_points <- loop_apex_points[with(loop_apex_points, order(loop)), ]

  # Aggregate multiple apex loop points into a single apex point feature
  loop_apex <- stats::aggregate(apex_points[c("POINT_X", "POINT_Y", "POINT_M",
                                              "DEM_Z", "v_POINT_X", "v_POINT_Y",
                                              "v_POINT_M")],
                                by = apex_points[c("loop")], FUN = mean)

  # Calculate coords of last record. Use as default to lead
  # to prevent NAs being introduced at the end of the series.
  downstream_x_lag   <- last(loop_apex$POINT_X)
  downstream_y_lag   <- last(loop_apex$POINT_Y)
  downstream_x_v_lag <- last(loop_apex$v_POINT_X)
  downstream_y_v_lag <- last(loop_apex$v_POINT_Y)

  # Calculate next downstream loop coordinates
  loop_apex$downstream_x <- lead(x = loop_apex$POINT_X,
                                 n = 1,
                                 default = downstream_x_lag)
  loop_apex$downstream_y <- lead(x = loop_apex$POINT_Y,
                                 n = 1,
                                 default = downstream_y_lag)

  # Calculate coordinates of the next downstream loop valley line
  loop_apex$downstream_v_x <- lead(x = loop_apex$v_POINT_X,
                                 n = 1,
                                 default = downstream_x_v_lag)
  loop_apex$downstream_v_y <- lead(x = loop_apex$v_POINT_Y,
                                 n = 1,
                                 default = downstream_y_v_lag)

  # Calculate current loop width
  loop_apex$current_loop_width <- pointDistance(p1 = cbind(loop_apex$POINT_X,
                                                           loop_apex$POINT_Y),
                                                p2 = cbind(loop_apex$v_POINT_X,
                                                           loop_apex$v_POINT_Y),
                                                lonlat = FALSE)

  # Calculate next loop width
  loop_apex$next_loop_width <- pointDistance(p1 = cbind(loop_apex$POINT_X,
                                                        loop_apex$POINT_Y),
                                             p2 = cbind(loop_apex$v_POINT_X,
                                                        loop_apex$v_POINT_Y),
                                             lonlat = FALSE)
  # Calculate meander width
  loop_apex$meander_width <- loop_apex$current_loop_width +
                             loop_apex$next_loop_width

  return(loop_apex)
}
