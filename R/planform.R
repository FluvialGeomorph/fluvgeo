#' @title Calculate planform dimensions
#'
#' @description Calculates planform dimensions (bend radius of curvature,
#' meander wavelength, and meander belt width) for each bend and loop in the
#' input `bankline_points` data frame.
#'
#' @export
#' @param bankline_points  data frame; a data frame of bankline points
#'
#' @return Returns a data frame of bends with the calculated planform
#' dimensions.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr lead lag
#' @importFrom raster pointDistance
#'
planform <- function(bankline_points) {
  # Check parameters
  assert_that(check_data_structure(bankline_points, "bankline_points"),
              msg = "'bankline_points' does not meet the data specification")

  # Calculate the bend radius of curvature
  bends <- bend_radius(bankline_points)

  # Calculate the meander length for each loop
  loop_length <- meander_length(bankline_points)

  # Calculate the meander width for each loop
  loop_width <- meander_width(bankline_points)

  # Join loop_length to bends
  bends_length <- merge(x = bends, y = loop_length, by = "loop", all = TRUE)

  # Join loop_width to bends
  bends_length_width <- merge(x = bends_length, y = loop_width, by = "loop",
                           all = TRUE)

  # Set fields to keep
  fields <- c("bend_num", "loop", "bend", "bend_POINT_X", "bend_POINT_Y",
              "bank_POINT_X.x", "bank_POINT_Y.x", "bend_radius",
              "meander_length", "meander_width")
  bends_planform <- bends_length_width[, fields]

  # Rename fields
  names(bends_planform)[names(bends_planform) == 'bank_POINT_X.x'] <- 'loop_POINT_X'
  names(bends_planform)[names(bends_planform) == 'bank_POINT_Y.x'] <- 'loop_POINT_Y'

  return(bends_planform)
}
