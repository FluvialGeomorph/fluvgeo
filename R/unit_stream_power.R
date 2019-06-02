#' @title Calculates unit stream power
#'
#' @description Calculates the unit stream power for the input cross sections.
#'
#' @export
#' @param reach_xs_dims   data frame; a data frame of cross section
#'                        dimensions.
#'
#' @return Returns a data frame of loops with the calculated meander width.
#'
#' @examples
#' # Extract data from the fgm::sin_xs_dimensions SpatialPointsDataFrame
#' sin_xs_dims_df <- fgm::sin_xs_dimensions@@data
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom stats aggregate
#' @importFrom dplyr last lead lag
#' @importFrom raster pointDistance
#'
unit_stream_power <- function(reach_xs_dims) {
  # Check parameters
  assert_that(check_data_structure(reach_xs_dims, "channel_feature"),
              msg = "'reach_xs_dims' does not meet the channel_feature data specification")
  assert_that(check_data_structure(reach_xs_dims, "slope_sinuosity"),
              msg = "'reach_xs_dims' does not meet the slope_sinuosity data specification")
  assert_that(check_data_structure(reach_xs_dims, "cross_section"),
              msg = "'reach_xs_dims' does not meet the cross section data specification")
  assert_that(check_data_structure(reach_xs_dims, "downhill"),
              msg = "'reach_xs_dims' does not meet the downhill data specification")



}
