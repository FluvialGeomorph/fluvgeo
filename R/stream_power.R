#' @title Calculate stream power for each cross section
#'
#' @description Calculates stream power for each cross section in the input
#'   xs_dims data frame.
#'
#' @export
#' @param xs_dims           data frame; A data frame of cross section
#'                          dimensions.
#' @param discharge_method  character; The method for calculating discharge (Q).
#'                          Must be one of: "model_measure", "regional_curve",
#'                          "width_relationship".
#' @param discharge_value   numeric; The discharge value (single value or vector)
#'                          to use for the stream power calculation. Required
#'                          if discharge_method = "model_measure".
#' @param region            character; The regional curve name used to calculate
#'                          discharge. Required if discharge_method =
#'                          "regional_curve". This parameter is passed to the
#'                          RegionalCurve::RHG function. See the RegionalCurve
#'                          package for a list of regions with discharge
#'                          relationships.
#' @param drainage_area     numeric; The drainage area (single value or vector)
#'                          used by the RegionalCurve::RHG function to calculate
#'                          discharge. Required if discharge_method =
#'                          "regional_curve".
#' @param width_method      character; The name of the width relationship used
#'                          to calculate discharge (Q) from width. Required if
#'                          discharge_method = "width_relationship". Must be
#'                          one of: <list goes here when implemented>
#'
#' @details
#'   \strong{Stream Power Equations}
#'   Stream power is the rate of energy dissipation against the bed and banks
#'   of a river or stream per unit downstream length. It is given by the
#'   equation:
#'
#'   \deqn{\Omega = \rho g Q S}
#'
#'   where \eqn{\Omega} is the stream power (kg/m/sec), \eqn{\rho} is the density of water
#'   (assumes 1000 kg/m^3), \eqn{g} is acceleration due to gravity (9.8 m/s2),
#'   \eqn{Q} is discharge (m^3/s), and \eqn{S} is the channel slope (m/m).
#'
#'   Unit stream power is stream power per unit channel width, and is given by
#'   the equation:
#'
#'   \deqn{\omega = (\rho g Q S) / b}
#'
#'   where \eqn{\omega} is the unit stream power, and \eqn{b} is the width of
#'   the channel (m).
#'
#'   \url{https://en.wikipedia.org/wiki/Stream_power}
#'
#'   \strong{Lane's Balance Equation}
#'   Stream power in the Lane's Balance equation is simply represented by
#'   discharge \eqn{Q} and slope \eqn{S}:
#'
#'   \deqn{\omega = Q S}
#'
#'   \strong{Determining Discharge}
#'   The \code{discharge_method} parameter specifies the source used for the
#'   discharge factor in the stream power equations.
#'
#'   \itemize{
#'     \item \code{model_measure} Use this option to supply an estimate of
#'     discharge from a hydraulic model or field measurement.
#'     \item \code{regional_curve} Use this option to use an estimate of
#'     discharge from a regional curve.
#'     \item \code{width_relationship} Use this option to use an estimate of
#'     discharge from a width relationship
#'   }
#'
#' @return Returns the input xs_dims data frame of cross sections with the
#'   calculated stream power variables added.
#'
#' @examples
#' # Calculate cross section dimensions
#' xs_dims <- cross_section_dimensions(xs = fluvgeo::sin_riffle_channel_sp,
#'                              xs_points = fluvgeo::sin_riffle_channel_points_sp,
#'                              bankfull_elevation = 103,
#'                              lead_n = 1,
#'                              use_smoothing = TRUE,
#'                              loess_span = 0.5)
#'
#' # Calculate stream power
#' xs_dims_sp <- stream_power(xs_dims,
#'                            discharge_method = "regional_curve",
#'                            region = "Illinois River",
#'                            drainage_area = 41)
#'
#' @importFrom assertthat assert_that
#'
stream_power <- function(xs_dims,
                         discharge_method = c("model_measure",
                                              "regional_curve",
                                              "width_relationship"),
                         discharge_value = NULL,
                         region = NULL,
                         drainage_area = NULL,
                         width_method = NULL) {


  # Check input parameters
  assert_that(is.data.frame(xs_dims),
              msg = "xs_dims must be a data frame")
  if(discharge_method == "model_measure" &
     is.null(discharge_value)) {
    stop(paste("If discharge_method = 'model_measure', parameter",
               "discharge_value must be specified."))
  }
  if(discharge_method == "regional_curve" &
     any(is.null(region), is.null(drainage_area))) {
    stop(paste("If discharge_method = 'regional_curve', parameters region and",
               "drainage_area must be specified."))
  }
  if(discharge_method == "width_relationship" &
     is.null(width_method)) {
    stop(paste("If discharge_method = 'width_relationship', parameter",
               "width_method must be specified."))
  }

  # Density of water (kg/m^3)
  rho <- 1000

  # Acceleration due to gravity (m/sec^2)
  g <- 9.8

  # Discharge
  Q <- switch(discharge_method,
              model_measure      = discharge_value,
              regional_curve     = RegionalCurve::RHG(region,
                                                      xs_dims$drainage_area,
                                                      "discharge"),
              width_relationship = 0,
              stop("discharge_method parameter is specified incorrectly"))

  # Set discharge
  xs_dims$discharge <- Q

  # Calculate stream power variables
  xs_dims$stream_power       <- rho * g * Q * xs_dims$slope
  xs_dims$stream_power_lane  <- Q * xs_dims$slope
  xs_dims$unit_stream_power  <- (rho * g * Q * xs_dims$slope) /
                                (xs_dims$xs_width * 0.3048)

  return(xs_dims)
}

