#' @title Calculate the meander length for each loop
#'
#' @description Calculates the meander length (wavelength) for each loop in the
#' input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  data frame; a data frame of bankline points
#'
#' @return Returns a data frame of loops with the calculated meander length.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr lead lag
#' @importFrom raster pointDistance
#'
meander_length <- function(bankline_points) {
  # Check parameters
  assert_that(check_data_structure(bankline_points, "bankline_points"),
              msg = "'channel_features' does not meet the data specification")

  # Select loop apex points
  loop_apex_points <- bankline_points[bankline_points$position == "apex", ]

  # Sort bankline_points by loop, bend, and POINT_M
  apex_points <- loop_apex_points[with(loop_apex_points, order(loop)), ]

  # Aggregate multiple apex loop points into a single apex point feature
  loop_apex <- aggregate(apex_points[c("POINT_X", "POINT_Y", "POINT_M", "DEM_Z",
                                       "v_POINT_X", "v_POINT_Y", "v_POINT_M")],
                         by = apex_points[c("loop")], FUN = mean)

  # Calculate coords of last record. Use as default to lead
  # to prevent NAs being introduced at the end of the series.
  downstream_x_lag <- last(loop_apex$POINT_X)
  downstream_y_lag <- last(loop_apex$POINT_Y)

  # Calculate coordinates of the second downstream loop
  loop_apex$downstream_x <- lead(x = loop_apex$POINT_X,
                             n = 2,
                             default = downstream_x_lag)
  loop_apex$downstream_y <- lead(x = loop_apex$POINT_Y,
                             n = 2,
                             default = downstream_y_lag)

  # Calculate meander length
  loop_apex$meander_length <- pointDistance(p1 = cbind(loop_apex$POINT_X,
                                                       loop_apex$POINT_Y),
                                            p2 = cbind(loop_apex$downstream_x,
                                                       loop_apex$downstream_y),
                                            lonlat = FALSE)

  return(loop_apex)
}
