#' @title Calculate planform dimensions for each cross section
#'
#' @description Calculates planform dimensions for each cross section in the
#'   input xs_dims data frame.
#'
#' @export
#' @param xs_dimensions   SpatialLinesDataFrame; A `cross_section_dimensions`
#'                        fluvgeo data structure.
#' @param bankline_points SpatialPointsDataFrame; A `bankline_points` fluvgeo data
#'                        data structure.
#'
#' @return Returns the input xs_dims data frame of cross sections with the
#'   calculated metric ratio variables added.
#'
#' @examples
#' # Calculate cross section planform dimensions
#' xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_sp,
#'                                    fluvgeo::sin_bankline_points_sp)
#' @importFrom sp merge
#'
planform_dimensions <- function(xs_dimensions, bankline_points) {
  # Check parameters
  check_cross_section_dimensions(xs_dimensions, "cross_section_dimensions")
  check_bankline_points(bankline_points)

  # Calculate planform dimensions
  bends_planform <- planform(bankline_points)

  # Join planform dimensions to xs_dimension
  xs_dims <- sp::merge(xs_dimensions, bends_planform,
                       by.x = c("loop", "bend"), by.y = c("loop", "bend"))

  return(xs_dims)
}
