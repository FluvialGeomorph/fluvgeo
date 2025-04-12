#' Calculate x and y coordinate values as new fields.
#'
#' @param points       sf object of type POINT, The input point features.
#' @param field_names  character, Provide custom names for the coordinate
#'                     fields to be created. Names must be provided in the
#'                     sf Dimension order (i.e,, XY, XTM, XYZ, XYZM).
#'
#' @details based on jmlondon suggestion for an `sf_as_cols` function:
#' [sf/issues/231](https://github.com/r-spatial/sf/issues/231)
#'
#' @return sf object of type POINT
#' @export
#'
#' @importFrom sf st_geometry st_coordinates
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#'
sf_point_attributes <- function(points, field_names = c("X", "Y")) {
  stopifnot(inherits(points, "sf") &&
            inherits(sf::st_geometry(points), "sfc_POINT"))

  coords <- sf::st_coordinates(points)
  coords <- tibble::as_tibble(coords)

  # remove duplicate names between points and field_names
  points <- points[, !names(points) %in% field_names]

  # assign field_names
  coords <- setNames(coords, field_names)
  dplyr::bind_cols(points, coords)
}
