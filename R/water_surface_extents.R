#' Channel and flood-prone water surface extents
#'
#' Extract unsimplified, flowline-connected inundation footprints from an REM
#' at two analyst-selected elevations. Interior holes are filled in the returned
#' outer footprints; the unmodified inundation polygons are also returned.
#'
#' @param rem A single-layer terra SpatRaster containing relative elevations.
#' @param flowline An sf flowline in the same coordinate reference system.
#' @param channel_elevation,floodplain_elevation Finite REM elevations in the
#'   raster's vertical units. The channel elevation must be strictly lower.
#' @return A list containing `channel`, `floodplain`, `channel_outer` and
#'   `floodplain_outer` sf polygons. The first two preserve interior holes;
#'   the outer footprints fill holes but do not bridge disconnected components.
#' @details This function does not simplify polygons. It uses the existing
#'   bathtub threshold and retains components intersecting the flowline. The
#'   footprints describe inundation, not measured flow paths or bank lines.
#' @export
water_surface_extents <- function(rem, flowline, channel_elevation,
                                  floodplain_elevation) {
  if (!inherits(rem, "SpatRaster") || terra::nlyr(rem) != 1L)
    stop("rem must be a single-layer SpatRaster.", call. = FALSE)
  if (!inherits(flowline, "sf") || !nrow(flowline) ||
      any(sf::st_is_empty(flowline)) ||
      !all(as.character(sf::st_geometry_type(flowline)) %in%
           c("LINESTRING", "MULTILINESTRING")))
    stop("flowline must contain nonempty sf line geometry.", call. = FALSE)
  if (is.na(sf::st_crs(flowline)) || is.na(sf::st_crs(rem)) ||
      sf::st_crs(flowline) != sf::st_crs(rem))
    stop("rem and flowline must have the same known CRS.", call. = FALSE)
  valid_level <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!valid_level(channel_elevation) || !valid_level(floodplain_elevation) ||
      channel_elevation >= floodplain_elevation)
    stop("Channel elevation must be finite and strictly less than floodplain elevation.", call. = FALSE)
  extent <- function(level) {
    mask <- terra::ifel(rem <= level, 1, NA)
    polys <- terra::disagg(terra::as.polygons(mask))
    if (nrow(polys) == 0L) stop("No inundation was found at the selected elevation.", call. = FALSE)
    result <- sf::st_as_sf(polys)
    result <- result[lengths(sf::st_intersects(result, flowline)) > 0L, , drop = FALSE]
    if (!nrow(result)) stop("No inundated area intersects the flowline.", call. = FALSE)
    sf::st_sf(water_surface_elevation = rep(as.numeric(level), nrow(result)),
              geometry = sf::st_geometry(result))
  }
  outer <- function(x) {
    p <- suppressWarnings(sf::st_cast(sf::st_geometry(x), "POLYGON"))
    filled <- sf::st_sfc(lapply(p, function(z) sf::st_polygon(list(z[[1L]]))),
                         crs = sf::st_crs(x))
    sf::st_sf(water_surface_elevation = rep(x$water_surface_elevation[1L], length(filled)),
              geometry = filled)
  }
  channel <- extent(channel_elevation)
  floodplain <- extent(floodplain_elevation)
  list(channel = channel, floodplain = floodplain,
       channel_outer = outer(channel), floodplain_outer = outer(floodplain))
}
