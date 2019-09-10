#' @title Calculate metric ratios for each cross section
#'
#' @description Calculates metric ratios for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dims           data frame; A data frame of cross section
#'                          dimensions.
#'
#' @return Returns the input xs_dims data frame of cross sections with the
#'   calculated metric ratio variables added.
#'
xs_metric_ratios <- function(xs_dims) {

  # Radius of curvature to bankfull width ratio
  xs_dims$rc_bfw_ratio <- (xs_dims$bend_radius * 3.28084) / xs_dims$xs_width

  # Meander belt width to bankfull width ratio
  xs_dims$mbw_bfw_ratio <- xs_dims$meander_width / xs_dims$xs_width

  return(xs_dims)
}
