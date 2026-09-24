#' Apply an existing aligned analysis mask to a terrain mosaic
#'
#' @param source Path to a single-band terrain mosaic.
#' @param mask Path to a one/NoData raster on the same CRS, resolution and cell
#'   alignment. Its extent must contain the source mosaic's extent.
#' @param filename New output GeoTIFF path in an existing directory.
#' @return Output path and grid/processing metadata. No study product is published.
#' @details Uses native terra crop and mask with file-backed output. Mask NoData
#'   cells become output NoData; retained source samples, datatype and units are
#'   unchanged. It neither resamples nor converts elevations. A source-grid
#'   mismatch requires explicit reconciliation before this operation. The saved
#'   mask is read, not regenerated. A compound elevation CRS and a 2D mask CRS
#'   are compared by their horizontal components. terra may report their full
#'   CRS difference; the source's full CRS and elevation units remain unchanged.
#'   No dataset-size admission limits are imposed.
#' @export
mask_terrain_mosaic <- function(source, mask, filename) {
  paths <- c(source, mask)
  if (length(source) != 1L || length(mask) != 1L || !is.character(paths) ||
      anyNA(paths) || any(!file.exists(paths)) || any(dir.exists(paths)))
    stop("Supply existing terrain and analysis mask raster paths.")
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !nzchar(filename) || file.exists(filename) || !dir.exists(dirname(filename)))
    stop("Supply a new output path in an existing directory.")
  x <- terra::rast(source)
  domain <- terra::rast(mask)
  horizontal <- function(path) {
    definition <- .fg_vertical_observe(path)$projjson
    if (identical(definition$type, "CompoundCRS")) {
      components <- Filter(function(z) z$type %in% c("ProjectedCRS", "GeographicCRS"), definition$components)
      if (length(components) != 1L) stop("Cannot identify one horizontal CRS component.")
      definition <- components[[1L]]
    }
    if (is.null(definition)) stop("Cannot read the raster's horizontal CRS.")
    sf::st_crs(as.character(jsonlite::toJSON(definition, auto_unbox = TRUE, digits = NA, null = "null")))
  }
  if (terra::nlyr(x) != 1L || terra::nlyr(domain) != 1L || !nzchar(terra::crs(x)) ||
      !isTRUE(horizontal(source) == horizontal(mask)) ||
      !terra::compareGeom(x, domain, crs = FALSE, ext = FALSE, rowcol = FALSE, res = TRUE,
                          stopOnError = FALSE) ||
      !isTRUE(all.equal(terra::origin(x), terra::origin(domain))))
    stop("The terrain and mask must share a single-band CRS, resolution and cell alignment.")
  xe <- as.vector(terra::ext(x)); me <- as.vector(terra::ext(domain))
  if (xe[1] < me[1] || xe[2] > me[2] || xe[3] < me[3] || xe[4] > me[4])
    stop("The saved mask extent must contain the terrain window.")
  temporary <- tempfile("mask-window-", tmpdir = dirname(filename), fileext = ".tif")
  on.exit(unlink(temporary), add = TRUE)
  cropped <- terra::crop(domain, terra::ext(x), filename = temporary,
                          wopt = list(datatype = "INT1U", NAflag = 255))
  if (!terra::compareGeom(x, cropped, crs = FALSE, stopOnError = FALSE))
    stop("The cropped mask does not match the terrain grid.")
  complete <- FALSE
  on.exit(if (!complete) unlink(filename), add = TRUE)
  output <- terra::mask(x, cropped, filename = filename,
    wopt = list(datatype = terra::datatype(x),
                gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")))
  reopened <- terra::rast(filename)
  if (!terra::compareGeom(x, reopened, stopOnError = FALSE) ||
      !identical(terra::datatype(x), terra::datatype(reopened)) ||
      !identical(terra::units(x), terra::units(reopened)))
    stop("Masked output metadata differs from the source grid, datatype or units.")
  complete <- TRUE
  list(path = normalizePath(filename, winslash = "/"),
       source = normalizePath(source, winslash = "/"), mask = normalizePath(mask, winslash = "/"),
       method = "terra::crop and terra::mask; no resampling or elevation conversion",
       crs = terra::crs(reopened), resolution = terra::res(reopened),
       extent = as.vector(terra::ext(reopened)), dimensions = dim(reopened),
       datatype = terra::datatype(reopened), units = terra::units(reopened),
       scientific_acceptance = FALSE)
}
