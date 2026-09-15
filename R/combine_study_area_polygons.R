#' Combine explicitly selected polygons into a Study Area boundary
#'
#' Dissolves internal borders using spherical geometry in WGS 84. Retains holes
#' and disconnected parts; does not repair, buffer, simplify, or clip geometry.
#' The result is a candidate for review, not a saved or accepted Study Area.
#' @param polygons An sf object with 1 to 200 valid, nonempty XY polygon features
#'   and a known CRS. Source attributes are not copied into the combined boundary.
#' @return List with boundary (one sf POLYGON or MULTIPOLYGON in EPSG:4326),
#'   selected_features and polygon_parts. No files are written.
#' @export
combine_study_area_polygons <- function(polygons) {
  if (!inherits(polygons, "sf") || nrow(polygons) < 1L || nrow(polygons) > 200L ||
      is.na(sf::st_crs(polygons))) stop("Select 1 to 200 CRS-defined polygons.", call. = FALSE)
  if (!all(as.character(sf::st_geometry_type(polygons)) %in% c("POLYGON", "MULTIPOLYGON")) ||
      !all(vapply(sf::st_geometry(polygons), inherits, logical(1), "XY")) ||
      any(sf::st_is_empty(polygons)) || !all(sf::st_is_valid(polygons) %in% TRUE))
    stop("Selected polygons must be valid nonempty XY geometry; no automatic repair is applied.", call. = FALSE)
  x <- sf::st_transform(sf::st_geometry(polygons), 4326)
  xy <- sf::st_coordinates(x)
  if (nrow(xy) > 1000000L || any(!is.finite(xy[, 1:2])) ||
      any(abs(xy[, 1]) > 180) || any(abs(xy[, 2]) > 90))
    stop("Selected geometry is invalid or too large for this boundary preview.", call. = FALSE)
  previous <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  combined <- sf::st_union(x)
  if (length(combined) != 1L || !isTRUE(sf::st_is_valid(combined)) || sf::st_is_empty(combined))
    stop("Combination did not produce a valid boundary; revise the selection.", call. = FALSE)
  list(boundary = sf::st_sf(geometry = combined), selected_features = nrow(polygons),
    polygon_parts = length(sf::st_cast(combined, "POLYGON")))
}
