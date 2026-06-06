#' Display unit helpers
#'
#' Internal helpers for resolving and formatting the user-facing display-unit
#' system used by plots, reports, legends, captions, and tables.
#'
#' This helper layer is designed to work in harmony with the open-source
#' geospatial ecosystem used by `fluvgeo`, especially `sf`, `terra`, `stars`,
#' and `units`. Package-facing functions should continue to expose the
#' repository's own `unit_system` contract while relying on ecosystem-native
#' unit objects and conversions where appropriate.
#'
#' @keywords internal
#' Validate a display unit system
#'
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return The validated unit system string.
#' @keywords internal
validate_unit_system <- function(unit_system) {
  assertthat::assert_that(
    is.character(unit_system),
    length(unit_system) == 1,
    unit_system %in% c("USCS", "SI"),
    msg = 'unit_system must be one of "USCS" or "SI"'
  )
  unit_system
}

#' Resolve a display unit specification
#'
#' @description
#' Convert a display unit system into a standardized specification list used by
#' output functions.
#'
#' The returned specification is intentionally package-facing, but it should be
#' interpreted in the context of the spatial unit infrastructure provided by
#' the open-source geospatial ecosystem. Where practical, package code should
#' use `units` objects and ecosystem-native conversions rather than ad hoc
#' scaling logic.
#'
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A named list containing display labels, abbreviations, conversion
#' helpers, and ecosystem-aligned unit metadata.
#' @keywords internal
unit_system_spec <- function(unit_system = "USCS") {
  unit_system <- validate_unit_system(unit_system)

  switch(
    unit_system,
    "USCS" = list(
      unit_system = "USCS",
      length_unit = "ft",
      area_unit = "sq ft",
      elevation_unit = "ft",
      distance_axis_label = "Distance (ft)",
      elevation_axis_label = "Elevation (ft)",
      area_label = "Area (sq ft)",
      vertical_reference_label = "NAVD88 ft",
      profile_distance_to_display = function(x) x * 3280.84,
      profile_distance_units = "ft",
      area_units = "ft^2",
      elevation_units = "ft",
      vertical_reference_units = "ft"
    ),
    "SI" = list(
      unit_system = "SI",
      length_unit = "m",
      area_unit = "sq m",
      elevation_unit = "m",
      distance_axis_label = "Distance (m)",
      elevation_axis_label = "Elevation (m)",
      area_label = "Area (sq m)",
      vertical_reference_label = "m",
      profile_distance_to_display = function(x) x * 1000,
      profile_distance_units = "m",
      area_units = "m^2",
      elevation_units = "m",
      vertical_reference_units = "m"
    )
  )
}

#' Format a display label for a unit-bearing quantity
#'
#' @param quantity character; the quantity name, such as "elevation" or
#'   "distance".
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A character scalar containing a display label.
#' @keywords internal
format_display_label <- function(quantity, unit_system = "USCS") {
  spec <- unit_system_spec(unit_system)

  switch(
    quantity,
    "distance" = spec$distance_axis_label,
    "elevation" = spec$elevation_axis_label,
    "area" = spec$area_label,
    "vertical_reference" = spec$vertical_reference_label,
    stop("Unknown display quantity: ", quantity, call. = FALSE)
  )
}

#' Convert a profile distance for display
#'
#' @param x numeric; profile distance in kilometers.
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return Numeric vector converted for display.
#' @keywords internal
convert_profile_distance <- function(x, unit_system = "USCS") {
  spec <- unit_system_spec(unit_system)
  spec$profile_distance_to_display(x)
}

#' Format a display unit string
#'
#' @param unit_system character; one of "USCS" or "SI".
#' @param quantity character; one of "distance", "area", "elevation",
#'   or "vertical_reference".
#'
#' @return A character scalar with the display unit string.
#' @keywords internal
format_display_units <- function(quantity, unit_system = "USCS") {
  spec <- unit_system_spec(unit_system)

  switch(
    quantity,
    "distance" = spec$profile_distance_units,
    "area" = spec$area_units,
    "elevation" = spec$elevation_units,
    "vertical_reference" = spec$vertical_reference_units,
    stop("Unknown display quantity: ", quantity, call. = FALSE)
  )
}

#' Convert a value to a units object for display
#'
#' @description
#' Create a `units` object using the display unit system. This is useful when a
#' plot, table, or report section needs to preserve explicit unit metadata
#' rather than relying only on numeric scaling.
#'
#' @param x numeric; values to attach display units to.
#' @param quantity character; one of "distance", "area", or "elevation".
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A `units` object.
#' @keywords internal
as_display_units <- function(x, quantity, unit_system = "USCS") {
  spec <- unit_system_spec(unit_system)

  unit_string <- switch(
    quantity,
    "distance" = spec$profile_distance_units,
    "area" = spec$area_units,
    "elevation" = spec$elevation_units,
    stop("Unknown display quantity: ", quantity, call. = FALSE)
  )

  units::set_units(x, unit_string, mode = "standard")
}

#' Convert a distance from kilometers to display units
#'
#' @param x numeric; profile distance in kilometers.
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return Numeric vector in display units.
#' @keywords internal
convert_distance_value <- function(x, unit_system = "USCS") {
  spec <- unit_system_spec(unit_system)
  spec$profile_distance_to_display(x)
}
