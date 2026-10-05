#' Smooth a terrain-derived Flowline
#'
#' Applies a bounded Gaussian-kernel smoother to one continuous Flowline while
#' preserving its endpoints and recording portable smoothing evidence.
#'
#' @param flowline A one-feature, projected `sf` LINESTRING.
#' @param bandwidth Positive numeric smoothing bandwidth in CRS map units. The
#'   default of 2 carries forward the legacy Flowline tool's PAEK tolerance.
#' @param max_displacement Maximum permitted Hausdorff displacement in CRS map
#'   units. Defaults to the smoothing bandwidth.
#' @return The smoothed `sf` Flowline with smoothing method, parameter, package,
#'   displacement, length-change, and validation fields.
#' @details The portable implementation uses [smoothr::smooth()] with Gaussian
#'   kernel regression and no unnecessary densification. It is not claimed to
#'   reproduce Esri PAEK vertices. The input coordinates remain the provenance
#'   record; callers should retain the raw selected path separately.
#' @export
smooth_flowline <- function(flowline, bandwidth = 2,
                            max_displacement = bandwidth) {
  if (!inherits(flowline, "sf") || nrow(flowline) != 1L ||
      is.na(sf::st_crs(flowline)) || sf::st_is_longlat(flowline) ||
      !identical(as.character(sf::st_geometry_type(flowline)), "LINESTRING") ||
      sf::st_is_empty(flowline) || !sf::st_is_valid(flowline) ||
      !sf::st_is_simple(flowline))
    stop("Supply one valid, simple, projected LINESTRING Flowline.")
  if (!is.numeric(bandwidth) || length(bandwidth) != 1L ||
      !is.finite(bandwidth) || bandwidth <= 0)
    stop("bandwidth must be one positive finite number in CRS map units.")
  if (!is.numeric(max_displacement) || length(max_displacement) != 1L ||
      !is.finite(max_displacement) || max_displacement <= 0)
    stop("max_displacement must be one positive finite number in CRS map units.")

  raw_coordinates <- sf::st_coordinates(flowline)[, c("X", "Y"), drop = FALSE]
  smoothed <- smoothr::smooth(flowline, method = "ksmooth",
    bandwidth = bandwidth, n = 1L)
  smooth_coordinates <- sf::st_coordinates(smoothed)[, c("X", "Y"), drop = FALSE]
  endpoints_equal <- isTRUE(all.equal(
    raw_coordinates[c(1L, nrow(raw_coordinates)), , drop = FALSE],
    smooth_coordinates[c(1L, nrow(smooth_coordinates)), , drop = FALSE],
    tolerance = sqrt(.Machine$double.eps), check.attributes = FALSE))
  if (!endpoints_equal || sf::st_is_empty(smoothed) ||
      !sf::st_is_valid(smoothed) || !sf::st_is_simple(smoothed))
    stop("Smoothing did not preserve a valid, simple Flowline with fixed endpoints.")

  displacement <- as.numeric(sf::st_distance(flowline, smoothed,
    which = "Hausdorff"))[1]
  if (!is.finite(displacement) || displacement > max_displacement)
    stop(sprintf("Smoothed Flowline displacement %.3f exceeds the %.3f map-unit limit.",
      displacement, max_displacement))
  raw_length <- as.numeric(sf::st_length(flowline))
  smooth_length <- as.numeric(sf::st_length(smoothed))
  unit <- sf::st_crs(flowline)$units_gdal
  if (is.null(unit) || is.na(unit) || !nzchar(unit)) unit <- "map units"

  smoothed$smoothing_method <- "Gaussian kernel regression"
  smoothed$smoothing_bandwidth <- bandwidth
  smoothed$smoothing_unit <- unit
  smoothed$smoothing_package <- paste0("smoothr ",
    as.character(utils::packageVersion("smoothr")))
  smoothed$maximum_displacement <- displacement
  smoothed$length_change_percent <- 100 * (smooth_length / raw_length - 1)
  smoothed$smoothing_valid <- TRUE
  if ("length_m" %in% names(smoothed)) {
    smoothed$raw_length_m <- smoothed$length_m
    smoothed$length_m <- as.numeric(units::set_units(sf::st_length(smoothed), "m"))
  }
  smoothed
}
