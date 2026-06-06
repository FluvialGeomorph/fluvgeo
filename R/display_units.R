#' Display unit helpers
#'
#' Internal helpers for resolving, formatting, and rendering the user-facing
#' display-unit system used by plots, reports, legends, captions, maps, static
#' documents, and interactive applications.
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

#' Render a unit-bearing label
#'
#' @param quantity character; one of "distance", "area", "elevation",
#'   or "vertical_reference".
#' @param unit_system character; one of "USCS" or "SI".
#' @param render character; one of "plain", "plotmath", "unicode", "prose",
#'   "latex", or "mathjax".
#'
#' @return A character scalar or expression-like string depending on render.
#' @keywords internal
render_unit_label <- function(quantity,
                              unit_system = "USCS",
                              render = c("plain", "plotmath", "unicode", "prose",
                                         "latex", "mathjax")) {
  render <- match.arg(render)
  spec <- unit_system_spec(unit_system)

  quantity_label <- switch(
    quantity,
    "distance" = "Distance",
    "area" = "Area",
    "elevation" = "Elevation",
    "vertical_reference" = "Vertical reference",
    stop("Unknown display quantity: ", quantity, call. = FALSE)
  )

  unit_string <- render_unit_symbol(quantity, unit_system, render)

  switch(
    render,
    "plain" = paste0(quantity_label, " (", unit_string, ")"),
    "plotmath" = paste0(quantity_label, " (", unit_string, ")"),
    "unicode" = paste0(quantity_label, " (", unit_string, ")"),
    "prose" = paste0(quantity_label, " (", unit_string, ")"),
    "latex" = paste0(quantity_label, " (", unit_string, ")"),
    "mathjax" = paste0(quantity_label, " (", unit_string, ")")
  )
}

#' Render a unit symbol
#'
#' @param quantity character; one of "distance", "area", "elevation",
#'   or "vertical_reference".
#' @param unit_system character; one of "USCS" or "SI".
#' @param render character; one of "plain", "plotmath", "unicode", "prose",
#'   "latex", or "mathjax".
#'
#' @return A character scalar containing the rendered unit symbol.
#' @keywords internal
render_unit_symbol <- function(quantity,
                               unit_system = "USCS",
                               render = c("plain", "plotmath", "unicode",
                                          "prose", "latex", "mathjax")) {
  render <- match.arg(render)
  spec <- unit_system_spec(unit_system)

  switch(
    quantity,
    "distance" = switch(
      render,
      "plain" = spec$profile_distance_units,
      "plotmath" = spec$profile_distance_units,
      "unicode" = spec$profile_distance_units,
      "prose" = spec$profile_distance_units,
      "latex" = spec$profile_distance_units,
      "mathjax" = spec$profile_distance_units
    ),
    "area" = switch(
      render,
      "plain" = spec$area_units,
      "plotmath" = spec$area_units,
      "unicode" = spec$area_units,
      "prose" = spec$area_units,
      "latex" = spec$area_units,
      "mathjax" = spec$area_units
    ),
    "elevation" = switch(
      render,
      "plain" = spec$elevation_units,
      "plotmath" = spec$elevation_units,
      "unicode" = spec$elevation_units,
      "prose" = spec$elevation_units,
      "latex" = spec$elevation_units,
      "mathjax" = spec$elevation_units
    ),
    "vertical_reference" = switch(
      render,
      "plain" = spec$vertical_reference_units,
      "plotmath" = spec$vertical_reference_units,
      "unicode" = spec$vertical_reference_units,
      "prose" = spec$vertical_reference_units,
      "latex" = spec$vertical_reference_units,
      "mathjax" = spec$vertical_reference_units
    ),
    stop("Unknown display quantity: ", quantity, call. = FALSE)
  )
}

#' Backwards-compatible label helper
#'
#' @param quantity character; one of "distance", "area", "elevation",
#'   or "vertical_reference".
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A character scalar containing a display label.
#' @keywords internal
format_display_label <- function(quantity, unit_system = "USCS") {
  render_unit_label(quantity, unit_system, render = "plain")
}

#' Backwards-compatible unit string helper
#'
#' @param quantity character; one of "distance", "area", "elevation",
#'   or "vertical_reference".
#' @param unit_system character; one of "USCS" or "SI".
#'
#' @return A character scalar with the display unit string.
#' @keywords internal
format_display_units <- function(quantity, unit_system = "USCS") {
  render_unit_symbol(quantity, unit_system, render = "plain")
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
