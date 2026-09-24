#' Convert NAVD88 metre elevations to international feet
#'
#' @param source Existing single-band GeoTIFF with explicit NAVD88 metre CRS
#'   (EPSG:5703) and metre band units.
#' @param filename New output GeoTIFF path in an existing directory.
#' @return Output path, grid, datatype, units and explicit conversion metadata.
#' @details Divides elevations by exactly 0.3048 using native terra arithmetic.
#'   Updates the vertical CRS to EPSG:8228 and the band unit to ft. Horizontal
#'   coordinates and the NAVD88 datum do not change. No resampling or geoid/datum
#'   transformation occurs. NoData is preserved. Original files are never changed.
#'   terra manages memory and temporary raster storage; no full raster values
#'   are extracted into R. Float32 inputs retain Float32 output storage.
#' @export
terrain_to_international_feet <- function(source, filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !nzchar(filename) || file.exists(filename) || !dir.exists(dirname(filename)))
    stop("Supply a new output path in an existing directory.")
  observed <- inspect_terrain_vertical_reference(source)
  definition <- observed$internal_compound$projjson
  vertical <- observed$internal_compound$vertical_crs
  if (!identical(definition$type, "CompoundCRS") ||
      !identical(vertical$id$authority, "EPSG") || !identical(as.character(vertical$id$code), "5703") ||
      !observed$internal_compound$band_unit %in% c("metre", "meter", "m"))
    stop("This conversion requires explicit NAVD88 metre reference and band units.")
  horizontal <- Filter(function(z) z$type %in% c("ProjectedCRS", "GeographicCRS"), definition$components)
  if (length(horizontal) != 1L) stop("Cannot identify one horizontal CRS component.")
  horizontal_wkt <- sf::st_crs(as.character(jsonlite::toJSON(horizontal[[1]],
    auto_unbox = TRUE, digits = NA, null = "null")))$wkt
  target_wkt <- sf::st_crs(paste0('COMPOUNDCRS["Terrain with NAVD88 international feet",',
    horizontal_wkt, ',', sf::st_crs("EPSG:8228")$wkt, ']'))$wkt
  x <- terra::rast(source)
  storage <- if (terra::datatype(x) == "FLT8S") "FLT8S" else "FLT4S"
  # Native raster arithmetic; metadata now describes the explicitly converted
  # values. This is not a coordinate transformation or a change to source files.
  converted <- x / 0.3048
  terra::crs(converted) <- target_wkt
  terra::units(converted) <- "ft"
  complete <- FALSE
  on.exit(if (!complete) unlink(filename), add = TRUE)
  terra::writeRaster(converted, filename, wopt = list(datatype = storage,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES", "BIGTIFF=IF_SAFER")))
  out <- terra::rast(filename)
  metadata <- .fg_vertical_observe(filename, internal = TRUE)
  if (!terra::compareGeom(x, out, crs = FALSE, stopOnError = FALSE) ||
      !isTRUE(sf::st_crs(terra::crs(out)) == sf::st_crs(target_wkt)) ||
      !metadata$band_unit %in% c("ft", "foot")) stop("Converted raster metadata did not reopen as expected.")
  complete <- TRUE
  list(path = normalizePath(filename, winslash = "/"), source = observed,
    method = "terra arithmetic: source metres / 0.3048", metre_per_foot = 0.3048,
    units = "international_foot", vertical_crs = "EPSG:8228", datum_operation = "none",
    crs = terra::crs(out), resolution = terra::res(out), extent = as.vector(terra::ext(out)),
    dimensions = dim(out), datatype = terra::datatype(out), scientific_acceptance = FALSE)
}
