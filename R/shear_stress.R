#' @title Calculate shear stress for each cross section
#'
#' @description Calculates shear stress for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dims             data frame; A data frame of cross section
#'                            dimensions.
#' @param specific_gravity    numeric; specific gravity of water
#'
#' @details
#'   \strong{Shear Stress Equations}
#'   Shear stress is a measure of the force of friction from a fluid acting on
#'   a body in the path of that fluid. In the case of open channel flow, it is
#'   the force of moving water against the bed of the channel. Shear stress is
#'   calculated as:
#'
#'   \deqn{\tau = \gamma D Sw}
#'
#'   where: \eqn{\tau} is the fluid shear stress (\eqn{N/m^2}), \eqn{\gamma}
#'   is the specific gravity of water (\eqn{N/m^3}), \eqn{D} is average water
#'   depth (m), \eqn{Sw} is the water surface slope (m/m).
#'
#'   \strong{Lane's Balance Equations}
#'   Shear stress in the
#'
#' @return Returns a data frame of cross sections with the calculated shear
#' stress.
#'
#' @examples
#' # Calculate cross section dimensions
#' xs_dims <- cross_section_dimensions(xs = fgm::sin_riffle_channel_sp,
#'                                     xs_points = fgm::sin_riffle_channel_points_sp,
#'                                     bankfull_elevation = 103,
#'                                     lead_n = 1,
#'                                     use_smoothing = TRUE,
#'                                     loess_span = 0.5)
#'
#' # Calculate shear stress
#' xs_dims_ss <- shear_stress(xs_dims)
#'
#' @importFrom assertthat assert_that
#'
shear_stress <- function(xs_dims, specific_gravity = 1) {
  # Calculate shear stress variables
  xs_dims$shear_stress <- specific_gravity * xs_dims$xs_depth * xs_dims$slope

  return(xs_dims)
}
