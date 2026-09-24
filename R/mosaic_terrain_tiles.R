#' Mosaic compatible elevation tiles on their existing grid
#'
#' Joins file-backed single-band tiles without projection, resampling, blending
#' or elevation conversion. The caller supplies source priority explicitly.
#' @param sources Ordered paths to compatible elevation rasters. The caller must
#'   establish common acquisition, elevation units and vertical reference.
#' @param filename New output GeoTIFF path. Existing files are never overwritten.
#' @param overlap Either `"first"` or `"last"` valid source value in overlap cells.
#' @param extent Optional numeric `c(xmin, xmax, ymin, ymax)` window in the source
#'   CRS. Native terra crop snaps outward to source cells before assembly. This
#'   is suitable for aligned, non-interpolated processing; callers must preserve
#'   any support needed by a later interpolation operation. NULL uses full tiles.
#' @return A compact list describing the output path, source order, grid and
#'   processing method. This is not scientific acceptance or a Survey Event product.
#' @details Uses native terra merge with missing values ignored and resampling
#'   disabled. Inputs must share their CRS, resolution and cell alignment. This
#'   primitive deliberately does not select a new analysis grid or reconcile
#'   vertical references. Output is file-backed Float32 with lossless compression.
#'   Use small windows of actual source DEMs for development. No processing-size
#'   limits are imposed. Applications should invoke it in a background worker.
#' @export
mosaic_terrain_tiles <- function(sources, filename, overlap, extent = NULL) {
  overlap <- match.arg(overlap, c("first", "last"))
  if (!is.character(sources) || !length(sources) || anyNA(sources) ||
      any(!file.exists(sources)) || any(dir.exists(sources)))
    stop("Supply existing source raster paths in priority order.")
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !nzchar(filename) || file.exists(filename) || !dir.exists(dirname(filename)))
    stop("Supply a new output path in an existing directory.")
  sources <- normalizePath(sources, winslash = "/", mustWork = TRUE)
  rasters <- lapply(sources, terra::rast)
  reference <- rasters[[1L]]
  if (!nzchar(terra::crs(reference)) || any(vapply(rasters, terra::nlyr, numeric(1)) != 1))
    stop("Tiles need a declared CRS and one elevation band.")
  for (r in rasters) {
    if (!terra::compareGeom(reference, r, crs = TRUE, ext = FALSE,
                            rowcol = FALSE, res = TRUE, stopOnError = FALSE) ||
        !isTRUE(all.equal(terra::origin(reference), terra::origin(r))))
      stop("Tiles do not share a source grid; explicit grid reconciliation is required.")
  }
  complete <- FALSE
  on.exit(if (!complete) unlink(filename), add = TRUE)
  options <- list(datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3",
                                             "TILED=YES", "BIGTIFF=IF_SAFER"))
  if (!is.null(extent)) {
    if (!is.numeric(extent) || length(extent) != 4L || any(!is.finite(extent)) ||
        extent[1] >= extent[2] || extent[3] >= extent[4])
      stop("Supply extent as finite xmin, xmax, ymin, ymax in the source CRS.")
    window <- terra::ext(extent)
    intersects <- vapply(rasters, function(r) {
      e <- as.vector(terra::ext(r))
      e[1] < extent[2] && e[2] > extent[1] && e[3] < extent[4] && e[4] > extent[3]
    }, logical(1))
    if (!any(intersects)) stop("No source tile intersects the requested extent.")
    rasters <- rasters[intersects]
    scratch <- tempfile("mosaic-crops-", tmpdir = dirname(filename))
    if (!dir.create(scratch)) stop("Cannot create mosaic crop staging.")
    on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
    rasters <- lapply(seq_along(rasters), function(i)
      terra::crop(rasters[[i]], window, snap = "out",
        filename = file.path(scratch, paste0(i, ".tif")), wopt = options))
    reference <- rasters[[1L]]
  }
  if (length(rasters) == 1L) {
    output <- terra::writeRaster(reference, filename, wopt = options)
  } else {
    output <- terra::merge(terra::sprc(rasters), first = overlap == "first",
                           na.rm = TRUE, algo = 1, resample = FALSE,
                           filename = filename, wopt = options)
  }
  reopened <- terra::rast(filename)
  if (!terra::compareGeom(output, reopened, stopOnError = FALSE) ||
      terra::datatype(reopened) != "FLT4S") stop("Mosaic output metadata did not reopen as written.")
  complete <- TRUE
  list(path = normalizePath(filename, winslash = "/"), sources = sources,
       overlap = overlap, requested_extent = extent,
       method = if (is.null(extent)) "terra::merge; no resampling or elevation conversion" else
         "terra::crop then terra::merge; no resampling or elevation conversion",
       crs = terra::crs(reopened), resolution = terra::res(reopened),
       extent = as.vector(terra::ext(reopened)), dimensions = dim(reopened),
       datatype = terra::datatype(reopened), terra_version = as.character(utils::packageVersion("terra")),
       scientific_acceptance = FALSE)
}
