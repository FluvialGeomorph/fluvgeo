#' @title Calculate the meander length for each loop
#'
#' @description Calculates the meander length (wavelength) for each loop in the
#' input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  SpatialPointsDataFrame; a fluvgeo bankline_points
#'                         data structure.
#'
#' @return Returns a data frame of loops with the calculated meander length in
#' units feet.
#'
#' @examples
#' meander_length(fluvgeo::sin_bankline_points_sp)
#'
#' @importFrom testthat expect_true
#' @importFrom stats aggregate
#' @importFrom dplyr last lead lag
#' @importFrom raster pointDistance
#'
meander_length <- function(bankline_points) {
  # Check parameters
  expect_true(check_bankline_points(bankline_points))

  # POINT_X, POINT_Y - X and Y horizontal units are in the units of the
  # channel_features' coordinate system. Distances calculated using them are
  # converted to feet in this function by the `horiz_con_factor`.

  # Set the horizontal unit conversion factor to calculate feet
  if(any(grep("units=m", sp::proj4string(bankline_points))) == 1) {
    horiz_con_factor <- 3.28084}
  if(any(grep("units=ft", sp::proj4string(bankline_points))) == 1) {
    horiz_con_factor <- 1}
  if(any(grep("units=us-ft", sp::proj4string(bankline_points))) == 1) {
    horiz_con_factor <- 0.999998000004}

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
  downstream_x_lag <- last(loop_apex$bank_POINT_X)
  downstream_y_lag <- last(loop_apex$bank_POINT_Y)

  # Calculate coordinates of the second downstream loop
  loop_apex$downstream_x <- lead(x = loop_apex$bank_POINT_X,
                             n = 2,
                             default = downstream_x_lag)
  loop_apex$downstream_y <- lead(x = loop_apex$bank_POINT_Y,
                             n = 2,
                             default = downstream_y_lag)

  # Calculate meander length
  loop_apex$meander_length <- pointDistance(p1 = cbind(loop_apex$bank_POINT_X,
                                                       loop_apex$bank_POINT_Y),
                                            p2 = cbind(loop_apex$downstream_x,
                                                       loop_apex$downstream_y),
                                            lonlat = FALSE) * horiz_con_factor

  return(loop_apex)
}
