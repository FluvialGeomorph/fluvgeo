#' @title Calculate the meander width for each loop
#'
#' @description Calculates the meander width (channel belt width) for each loop
#' in the input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  sf data.frame; a fluvgeo bankline_points data
#'                         structure
#'
#' @return Returns a data frame of loops with the calculated meander width in
#' units feet.
#'
#' @examples
#' meander_width(fluvgeo::sin_bankline_points_sf)
#'
#' @importFrom testthat expect_true
#' @importFrom stats aggregate
#' @importFrom dplyr last lead lag
#' @importFrom raster pointDistance
#'
meander_width <- function(bankline_points) {
  # Check parameters
  expect_true(check_bankline_points(bankline_points))

  # POINT_X, POINT_Y - X and Y horizontal units are in the units of the
  # channel_features' coordinate system. Distances calculated using them are
  # converted to feet in this function by the `horiz_con_factor`.

  # Set the horizontal unit conversion factor to calculate feet
  if(any(grep("metre",sf::st_crs(bankline_points, parameters=TRUE)$units_gdal)) == 1) {
    horiz_con_factor <- 3.28084}
  if(any(grep("foot", sf::st_crs(bankline_points, parameters=TRUE)$units_gdal)) == 1) {
    horiz_con_factor <- 1}
  if(any(grep("US survey foot",sf::st_crs(bankline_points, parameters=TRUE)$units_gdal)) == 1) {
    horiz_con_factor <- 0.999998000004}

  # Convert sf data frame to a data frame
  bankline_points <- data.frame(bankline_points)

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
                                                lonlat = FALSE) * horiz_con_factor

  # Calculate next loop width
  loop_apex$next_loop_width <- pointDistance(p1 = cbind(loop_apex$bank_POINT_X,
                                                        loop_apex$bank_POINT_Y),
                                             p2 = cbind(loop_apex$valley_POINT_X,
                                                        loop_apex$valley_POINT_Y),
                                             lonlat = FALSE) * horiz_con_factor
  # Calculate meander width
  loop_apex$meander_width <- loop_apex$current_loop_width +
                             loop_apex$next_loop_width

  return(loop_apex)
}
