#' @title Calculate shear stress for each cross section
#'
#' @description Calculates shear stress for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dims             data frame; A data frame of cross section
#'                            dimensions.
#' @param specific_gravity    numeric; specific gravity of water (units N/m^3)
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
#'   where: \eqn{\tau} is the fluid shear stress (\eqn{N/m^2, lb/ft^2}),
#'   \eqn{\gamma} is the specific gravity of water (\eqn{N/m^3, lb/ft^3}),
#'   \eqn{D} is average water depth (m, ft), \eqn{Sw} is the water surface
#'   slope (m/m, ft/ft).
#'
#'   \strong{Lane's Balance Equations}
#'   Shear stress in the
#'
#' @return Returns a data frame of cross sections with the calculated shear
#' stress.
#'
#' @examples
#' # Calculate cross section dimensions
#' xs_dims <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
#'                                     xs_points = fluvgeo::sin_riffle_channel_points_sp,
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
  # Convert specific_gravity from N/m^3 to lb/ft^3
  sg_imperial <- specific_gravity * 157.0874606377

  # Calculate shear stress variables
  xs_dims$shear_stress <- specific_gravity * (xs_dims$xs_depth * 0.3048) * xs_dims$slope

  xs_dims$shear_stress_imperial <- sg_imperial * (xs_dims$xs_depth) * xs_dims$slope

  return(xs_dims)
}
