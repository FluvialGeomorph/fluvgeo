#' @title Calculate shear stress for each cross section
#'
#' @description Calculates shear stress for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dims             data frame; A data frame of cross section
#'                            dimensions.
#' @param water_density       numeric; density of water (units kg/m^3)
#'
#' @details
#'   \strong{Shear Stress Equations}
#'   Shear stress is a measure of the force of friction from a fluid acting on
#'   a body in the path of that fluid. In the case of open channel flow, it is
#'   the force of moving water against the bed of the channel. Generalized
#'   shear stress or tractive force is commonly defined using the following
#    equations:
#'
#'   \strong{Density of Water}
#'   This functional form of the equation defines the \eqn{\gamma} term as the
#'   density of water.
#'
#'   \deqn{\tau = \gamma D Sw}
#'
#'   where:
#'   \eqn{\tau} is the fluid shear stress (\eqn{kg/m^2}),
#'   \eqn{\gamma} is the density of water (\eqn{kg/m^3}),
#'   \eqn{D} is mean water depth (m),
#'   \eqn{Sw} is the water surface slope (dimensionless: m/m).
#'
#'
#'   \strong{Specific Weight of Water}
#'   This functional form of the equation defines the \eqn{\gamma} term as the
#'   specific weight of water
#'
#'   \deqn{\tau = \gamma D Sw}
#'
#'   where:
#'   \eqn{\tau} is the fluid shear stress (\eqn{lb/ft^2}),
#'   \eqn{\gamma} is the specific weight of water (\eqn{lb/ft^2}),
#'   \eqn{D} is mean water depth (ft),
#'   \eqn{Sw} is the water surface slope (dimensionless: ft/ft).
#'
#'   where:
#'   \eqn{\gamma = \rho a_{g}}
#'
#'   \eqn{\rho} is the density of water (\eqn{slugs/ft^3}),
#'   \eqn{a_{g}} is the acceleration of gravity (\eqn{ft/sec^2})
#'
#'
#'   \strong{Lane's Balance Version}
#'   This functional form of the equation ignores absolute values and simply
#'   focuses on the variables that are changing:
#'
#'   \deqn{\tau = D Sw}
#'
#'   \eqn{D} is mean water depth (m),
#'   \eqn{Sw} is the water surface slope (dimensionless: m/m).
#'
#'
#'   \strong{Typical Values}
#'   \itemize{
#'     \item Density of water at \eqn{4^{\circ} C (39^{\circ} F)} = \eqn{1000 kg/m^3}
#'     or \eqn{1.94032 slugs/ft^3}
#'     \item Acceleration of gravity = \eqn{9.807 m/s^2, 32.174 ft/s^2}
#'   }
#'
#'
#' @return Returns a data frame of cross sections with the calculated shear
#' stress.
#'
#' \itemize{
#'     \item \code{shear_stress_density} Shear stress calculated using the
#'     density of water typically of the form "1000 D Sw". Units: \eqn{kg/m^2}
#'     \item \code{shear_stress_weight} Shear stress calculated using the
#'     specific weight of water. Units: \eqn{lb/ft^2}
#'     \item \code{shear_stress_lane} Shear stress calculated using just mean
#'     depth and water surface slope. Units:
#'   }
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
shear_stress <- function(xs_dims, water_density = 1000) {
  # Convert xs_depth (calculated as max. depth) to mean_depth
  # mean_depth = xs_area * xs_width
  xs_dims$mean_depth <- xs_dims$xs_area * xs_dims$xs_width

  # Convert water_density from kg/m^3 to slugs/ft^3
  # 1 kg/m^3 = 0.00194032 slugs/ft^3
  water_density_slug <- water_density * 0.00194032

  # Calculate the specific weight of water (lb/ft^2)
  # specific_weight = water density * acceleration of gravity
  specific_weight <- water_density_slug * 32.174

  # Calculate shear stress variables
  xs_dims$shear_stress_density <- water_density * (xs_dims$xs_depth * 0.3048) * xs_dims$slope

  xs_dims$shear_stress_weight <- specific_weight * xs_dims$xs_depth * xs_dims$slope

  xs_dims$shear_stress_lane <- xs_dims$xs_depth * xs_dims$slope

  return(xs_dims)
}
