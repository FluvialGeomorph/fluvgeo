#' @title Calculate metric ratios for each cross section
#'
#' @description Calculates metric ratios for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dimensions   SpatialLinesDataFrame; A `cross_section_dimensions`
#'                        fluvgeo data structure.
#'
#' @return Returns the input xs_dims data frame of cross sections with the
#'   calculated metric ratio variables added.
#'
#' @examples
#' # Calculate cross section planform dimensions
#' xs_dims_plan <- planform_dimensions(fluvgeo::sin_riffle_floodplain_dims_sp,
#'                                     fluvgeo::sin_bankline_points_sp)
#'
#' # Calculate cross section metric ratios
#' xs_dims_ratios <- xs_metric_ratios(xs_dims_plan)
#'
#'
xs_metric_ratios <- function(xs_dimensions) {
  # Check parameters
  check_cross_section_dimensions(xs_dimensions, "planform")

  # Radius of curvature to bankfull width ratio
  xs_dimensions$rc_bfw_ratio <- (xs_dimensions$bend_radius * 3.28084) /
                                 xs_dimensions$xs_width

  # Radius of curvature to bankfull width ratio <= 10
  xs_dimensions$rc_bfw_ratio_lte_10 <- xs_dimensions$rc_bfw_ratio
  xs_dimensions$rc_bfw_ratio_lte_10[xs_dimensions$rc_bfw_ratio > 10] <- 10

  # Meander belt width to bankfull width ratio
  xs_dimensions$mbw_bfw_ratio <- xs_dimensions$meander_width /
                                 xs_dimensions$xs_width

  # Meander belt width to bankfull width ratio <= 50
  xs_dimensions$mbw_bfw_ratio_lte_30 <- xs_dimensions$mbw_bfw_ratio
  xs_dimensions$mbw_bfw_ratio_lte_30[xs_dimensions$mbw_bfw_ratio > 30] <- 30

  return(xs_dimensions)
}
