#' Display unit helpers
#'
#' Internal helpers for resolving and formatting the user-facing display-unit
#' system used by plots, reports, legends, captions, and tables.
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
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A named list containing display labels, abbreviations, and display
#' conversion helpers.
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
      profile_distance_to_display = function(x) x * 3280.84
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
      profile_distance_to_display = function(x) x * 1000
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
