#' @title Calculate the meander width for each loop
#'
#' @description Calculates the meander width (channel belt width) for each loop
#' in the input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  SpatialPointsDataFrame; an fluvgeo bankline_points data
#'                         structure
#'
#' @return Returns a data frame of loops with the calculated meander width.
#'
#' @examples
#' meander_width(fluvgeo::sin_bankline_points_sp)
#'
#' @importFrom testthat expect_true
#' @importFrom stats aggregate
#' @importFrom dplyr last lead lag
#' @importFrom raster pointDistance
#'
meander_width <- function(bankline_points) {
  # Check parameters
  expect_true(check_bankline_points(bankline_points))

  # Convert Spatial*DataFrame to a data frame
  bankline_points <- bankline_points@data

  # Select loop apex points
  loop_apex_points <- bankline_points[bankline_points$position == "apex", ]

  # Sort bankline_points by loop, bend, and POINT_M
  apex_points <- loop_apex_points[with(loop_apex_points, order(loop)), ]

  # Aggregate multiple apex loop points into a single apex point feature
  loop_apex <- stats::aggregate(apex_points[c("bank_POINT_X", "bank_POINT_Y",
                                              "bank_POINT_M",
                                              "DEM_Z",
                                              "valley_POINT_X", "valley_POINT_Y",
                                              "valley_POINT_M")],
                                by = apex_points[c("loop")], FUN = mean)

  # Calculate coords of last record. Use as default to lead
  # to prevent NAs being introduced at the end of the series.
  downstream_x_lag   <- last(loop_apex$bank_POINT_X)
  downstream_y_lag   <- last(loop_apex$bank_POINT_Y)
  downstream_x_v_lag <- last(loop_apex$valley_POINT_X)
  downstream_y_v_lag <- last(loop_apex$valley_POINT_Y)

  # Calculate next downstream loop coordinates
  loop_apex$downstream_x <- lead(x = loop_apex$bank_POINT_X,
                                 n = 1,
                                 default = downstream_x_lag)
  loop_apex$downstream_y <- lead(x = loop_apex$bank_POINT_Y,
                                 n = 1,
                                 default = downstream_y_lag)

  # Calculate coordinates of the next downstream loop valley line
  loop_apex$downstream_v_x <- lead(x = loop_apex$valley_POINT_X,
                                 n = 1,
                                 default = downstream_x_v_lag)
  loop_apex$downstream_v_y <- lead(x = loop_apex$valley_POINT_Y,
                                 n = 1,
                                 default = downstream_y_v_lag)

  # Calculate current loop width
  loop_apex$current_loop_width <- pointDistance(p1 = cbind(loop_apex$bank_POINT_X,
                                                           loop_apex$bank_POINT_Y),
                                                p2 = cbind(loop_apex$valley_POINT_X,
                                                           loop_apex$valley_POINT_Y),
                                                lonlat = FALSE)

  # Calculate next loop width
  loop_apex$next_loop_width <- pointDistance(p1 = cbind(loop_apex$bank_POINT_X,
                                                        loop_apex$bank_POINT_Y),
                                             p2 = cbind(loop_apex$valley_POINT_X,
                                                        loop_apex$valley_POINT_Y),
                                             lonlat = FALSE)
  # Calculate meander width
  loop_apex$meander_width <- loop_apex$current_loop_width +
                             loop_apex$next_loop_width

  return(loop_apex)
}
