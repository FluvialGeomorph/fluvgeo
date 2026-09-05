#' Orient lines upstream using DEM endpoint elevations
#'
#' Generalizes the endpoint comparison used by flowline(). Finite, unequal
#' endpoint elevations support downstream-to-upstream coordinate order: keep
#' a line whose start is lower, or reverse a line whose end is lower. Elevations
#' are sampled from their containing raster cells (terra's simple extraction).
#' No elevation-difference threshold or profile inference is introduced.
#'
#' Equal or missing endpoint elevations and true multipart geometries remain
#' unchanged and unresolved. This method does not establish network topology
#' or guarantee downhill flow throughout a line. The caller must supply the
#' appropriate source DEM. Values use its native vertical units; a file reference
#' is recorded when available, but is not a content fingerprint.
#'
#' @param lines Nonempty projected sf line features. Attributes, row order, CRS,
#'   and geometry types are preserved. MULTILINESTRING rows with one part are
#'   supported; rows with several parts require separate normalization.
#' @param dem Single-band terra SpatRaster in the same CRS as lines.
#'
#' @return A list with lines (the resulting sf) and direction (one evidence row
#'   per input row). Evidence fields are input_row, start_elevation, end_elevation
#'   (before correction), start_sample_status, end_sample_status (AVAILABLE,
#'   OUTSIDE_DEM_EXTENT, DEM_NODATA, or NOT_SAMPLED),
#'   action (KEEP, REVERSE, UNRESOLVED), reason_code,
#'   method, dem_band, and dem_source. No governed identifiers are required.
#' @export
orient_lines_from_dem <- function(lines, dem) {
  if (!inherits(lines, "sf") || !nrow(lines)) {
    .fg_abort("`lines` must be a nonempty sf object.")
  }
  if (is.na(sf::st_crs(lines)) || !identical(sf::st_is_longlat(lines), FALSE)) {
    .fg_abort("`lines` must have a projected CRS.")
  }
  types <- as.character(sf::st_geometry_type(lines))
  if (any(!types %in% c("LINESTRING", "MULTILINESTRING")) ||
      any(sf::st_is_empty(lines)) || !all(sf::st_is_valid(lines))) {
    .fg_abort("`lines` must contain valid, nonempty line geometry.")
  }
  if (!inherits(dem, "SpatRaster") || terra::nlyr(dem) != 1L) {
    .fg_abort("`dem` must be a single-band terra SpatRaster.")
  }
  if (is.na(sf::st_crs(dem)) || !isTRUE(sf::st_crs(lines) == sf::st_crs(dem))) {
    .fg_abort("`lines` and `dem` must have the same CRS.")
  }
  geometry <- sf::st_geometry(lines)
  single <- types == "LINESTRING" | lengths(geometry) == 1L
  selected <- which(single)
  start_z <- end_z <- rep(NA_real_, nrow(lines))
  start_status <- end_status <- rep("NOT_SAMPLED", nrow(lines))
  if (length(selected)) {
    coordinates <- lapply(selected, function(i) {
      line <- if (types[i] == "MULTILINESTRING") geometry[[i]][[1L]] else geometry[[i]]
      unclass(line)[c(1L, nrow(line)), 1:2, drop = FALSE]
    })
    xy <- do.call(rbind, coordinates)
    z <- terra::extract(dem, xy, method = "simple")[[1L]]
    cells <- terra::cellFromXY(dem, xy)
    sample_status <- ifelse(is.na(cells), "OUTSIDE_DEM_EXTENT",
                            ifelse(is.finite(z), "AVAILABLE", "DEM_NODATA"))
    start_z[selected] <- z[seq.int(1L, length(z), by = 2L)]
    end_z[selected] <- z[seq.int(2L, length(z), by = 2L)]
    start_status[selected] <- sample_status[seq.int(1L, length(z), by = 2L)]
    end_status[selected] <- sample_status[seq.int(2L, length(z), by = 2L)]
  }
  supported <- single & is.finite(start_z) & is.finite(end_z) & start_z != end_z
  action <- rep("UNRESOLVED", nrow(lines))
  action[which(supported & start_z < end_z)] <- "KEEP"
  reverse <- which(supported & start_z > end_z)
  action[reverse] <- "REVERSE"
  reason <- rep("MULTIPART_GEOMETRY", nrow(lines))
  reason[single] <- "ENDPOINT_DEM_NODATA"
  reason[start_status == "OUTSIDE_DEM_EXTENT" | end_status == "OUTSIDE_DEM_EXTENT"] <-
    "ENDPOINT_OUTSIDE_DEM"
  reason[which(single & is.finite(start_z) & is.finite(end_z) & start_z == end_z)] <-
    "EQUAL_ENDPOINT_ELEVATION"
  reason[which(supported)] <- "DEM_ENDPOINT_ORDER"

  for (i in reverse) {
    reversed <- sf::st_geometry(sf_line_reverse(lines[i, ]))
    geometry[[i]] <- if (types[i] == "MULTILINESTRING") {
      sf::st_multilinestring(list(unclass(reversed[[1L]])),
                            dim = class(geometry[[i]])[1L])
    } else reversed[[1L]]
  }
  output <- sf::st_set_geometry(lines, geometry)
  dem_source <- terra::sources(dem)
  dem_source <- dem_source[!is.na(dem_source) & nzchar(dem_source)]
  list(
    lines = output,
    direction = tibble::tibble(
      input_row = seq_len(nrow(lines)),
      start_elevation = start_z,
      end_elevation = end_z,
      start_sample_status = start_status,
      end_sample_status = end_status,
      action = action,
      reason_code = reason,
      method = "DEM_ENDPOINTS_1",
      dem_band = names(dem)[1L],
      dem_source = if (length(dem_source)) paste(dem_source, collapse = ";") else NA_character_
    )
  )
}
