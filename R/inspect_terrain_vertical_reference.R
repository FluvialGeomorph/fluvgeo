#' Inspect vertical-reference declarations in a local GeoTIFF
#'
#' Compares the current GDAL reader view with an internal-only read that requests
#' preservation of a compound CRS. A declaration can exist without being exposed
#' by the ordinary reader. Neither observation establishes the reference actually
#' used to produce the raster elevations or scientific acceptance.
#'
#' @param path Path to an existing local, single-band native GeoTIFF.
#' @return A list with schema, normalized path, SHA-256, software versions,
#'   default_reader and internal_compound observations, and crs_text_differs.
#'   Each observation contains status, WKT, optional PROJJSON, optional vertical
#'   CRS component, band unit, and the requested reader options. Status is
#'   VERTICAL_CRS_EXPOSED or VERTICAL_CRS_NOT_EXPOSED, never proof of absence.
#'   Text differences require review, not automatic conflict resolution.
#' @details No statistics, raster processing, metadata assignment or file writes
#'   are requested. The file is hashed before and after inspection (two full byte
#'   reads). GDAL errors and unsupported inputs fail, rather than becoming unknown
#'   metadata. The internal read disables PAM sidecars; the ordinary read may use
#'   them. Options are scoped to each call. Existing intake manifests are unchanged.
#'   A three-dimensional geodetic CRS is retained but is not classified as a
#'   separate vertical CRS or automatically associated with raster band values.
#' @seealso [inspect_terrain_folder()], [record_study_terrain_metadata()]
#' @md
#' @export
inspect_terrain_vertical_reference <- function(path) {
  path <- .fg_required_text(path, "path")
  if (!file.exists(path) || dir.exists(path) ||
      !grepl("\\.tiff?$", path, ignore.case = TRUE))
    .fg_abort("Supply an existing local GeoTIFF file.")
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  con <- file(path, "rb")
  signature <- tryCatch(paste(format(readBin(con, "raw", n = 4L)), collapse = ""),
                        finally = close(con))
  if (!signature %in% c("49492a00", "4d4d002a", "49492b00", "4d4d002b"))
    .fg_abort("Expected a native TIFF, not a renamed virtual raster or other format.")
  if (!"config_options" %in% names(formals(sf::gdal_utils)))
    .fg_abort("This inspection requires sf::gdal_utils with scoped config_options support.")
  before <- .fg_file_sha256(path)
  ordinary <- .fg_vertical_observe(path)
  internal <- .fg_vertical_observe(path, internal = TRUE)
  if (!identical(before, .fg_file_sha256(path)))
    .fg_abort("Artifact changed during vertical-reference inspection.")
  list(schema = "FLUVGEO_VERTICAL_REFERENCE_OBSERVATION_1", path = path,
    sha256 = before,
    software = list(fluvgeo = as.character(utils::packageVersion("fluvgeo")),
      sf = as.character(utils::packageVersion("sf")),
      geospatial = as.list(sf::sf_extSoftVersion())),
    default_reader = ordinary, internal_compound = internal,
    crs_text_differs = !identical(ordinary$wkt, internal$wkt))
}

.fg_vertical_observe <- function(path, internal = FALSE) {
  options <- c("-json", "-norat")
  config <- character()
  if (internal) {
    options <- c(options, "-oo", "GEOREF_SOURCES=INTERNAL")
    config <- c(GTIFF_REPORT_COMPD_CS = "TRUE", GDAL_PAM_ENABLED = "NO")
  }
  raw <- sf::gdal_utils("info", path, options = options,
                        config_options = config, quiet = TRUE)
  x <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
  if (!identical(x$driverShortName, "GTiff") || length(x$bands) != 1L)
    .fg_abort("Vertical-reference inspection supports single-band GeoTIFF only.")
  wkt <- if (is.null(x$coordinateSystem$wkt)) "" else x$coordinateSystem$wkt
  projjson <- x$stac[["proj:projjson"]]
  vertical <- .fg_vertical_component(projjson)
  exposed <- !is.null(vertical) || grepl("(^|[[:space:],])(VERTCRS|VERT_CS)\\[", wkt)
  list(status = if (exposed) "VERTICAL_CRS_EXPOSED" else "VERTICAL_CRS_NOT_EXPOSED",
    wkt = wkt, projjson = projjson, vertical_crs = vertical,
    band_unit = if (is.null(x$bands[[1L]]$unit)) "" else x$bands[[1L]]$unit,
    open_options = if (internal) "GEOREF_SOURCES=INTERNAL" else character(),
    config_options = as.list(config))
}

.fg_vertical_component <- function(crs) {
  if (is.null(crs)) return(NULL)
  if (identical(crs$type, "VerticalCRS")) return(crs)
  if (identical(crs$type, "BoundCRS")) return(.fg_vertical_component(crs$source_crs))
  if (identical(crs$type, "CompoundCRS")) {
    for (component in crs$components) {
      vertical <- .fg_vertical_component(component)
      if (!is.null(vertical)) return(vertical)
    }
  }
  NULL
}
