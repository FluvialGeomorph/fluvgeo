#' Mosaic compatible elevation tiles with optional output-grid resampling
#'
#' Joins file-backed single-band tiles in source priority order, optionally
#' resampling differing source grids to a same-CRS output template.
#' @param sources Ordered paths to compatible elevation rasters. The caller must
#'   establish common acquisition, elevation units and vertical reference.
#' @param filename New output GeoTIFF path. Existing files are never overwritten.
#' @param overlap Either `"first"` or `"last"` valid source value in overlap cells.
#' @param extent Optional numeric `c(xmin, xmax, ymin, ymax)` window in the source
#'   CRS. Native terra crop snaps outward to source cells before assembly. This
#'   is suitable for aligned, non-interpolated processing. Without a template,
#'   callers preserve support for any later interpolation; NULL uses full tiles.
#'   With a template, extent restricts its grid and source support is automatic.
#' @param template Optional single-band raster path defining the output grid in
#'   the same horizontal CRS. Bilinear resampling follows source-grid assembly;
#'   template values are ignored. With extent, snap the target window outward
#'   to template cells. Source reads retain a two-cell interpolation halo at the
#'   larger of source and target spacing. Apply the analysis mask afterwards.
#' @return A compact list describing the output path, source order, grid and
#'   processing method. This is not scientific acceptance or a Survey Event product.
#' @details Inputs share full CRS and elevation units. Without a template they
#'   must also share resolution and alignment. With a template, consecutive
#'   same-grid tiles are joined before bilinear resampling to the exact Event
#'   grid. Resampled runs are merged in source order with first/last-valid
#'   precedence and no additional interpolation. NoData follows native terra
#'   semantics; no blending, gap filling or reference conversion is added.
#'   Each run uses the full target template to keep native interpolation edge
#'   weights consistent; source reads remain windowed with interpolation support.
#'   Output is file-backed Float32 with lossless compression.
#'   Use small windows of actual source DEMs for development. No processing-size
#'   limits are imposed. Applications should invoke it in a background worker.
#' @export
mosaic_terrain_tiles <- function(sources, filename, overlap, extent = NULL, template = NULL) {
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
  same_grid <- function(a,b) terra::compareGeom(a,b,crs=FALSE,ext=FALSE,
    rowcol=FALSE,res=TRUE,stopOnError=FALSE) &&
    isTRUE(all.equal(terra::origin(a),terra::origin(b)))
  for (r in rasters) {
    if (!isTRUE(sf::st_crs(terra::crs(reference)) == sf::st_crs(terra::crs(r))) ||
        !identical(terra::units(reference),terra::units(r)))
      stop("Source tiles must share their full CRS and elevation units; reference reconciliation is required.")
  }
  mixed <- !all(vapply(rasters,function(r) same_grid(reference,r),logical(1)))
  if (mixed && is.null(template))
    stop("Tiles do not share a source grid; supply an output-grid template for resampling.")
  if (!is.null(template)) {
    if (!is.character(template) || length(template) != 1L || is.na(template) ||
        !file.exists(template) || dir.exists(template)) stop("Supply an existing output-grid template.")
    # Only template geometry is needed; do not read/crop its mask pixel values.
    target <- terra::rast(terra::rast(template))
    horizontal <- function(path) {
      j <- .fg_vertical_observe(path)$projjson
      if (identical(j$type, "CompoundCRS")) {
        components <- Filter(function(z) identical(z$type, "ProjectedCRS"), j$components)
        if (length(components) != 1L) stop("A projected horizontal CRS is required.")
        j <- components[[1L]]
      }
      if (!identical(j$type, "ProjectedCRS")) stop("A projected horizontal CRS is required.")
      sf::st_crs(as.character(jsonlite::toJSON(j, auto_unbox = TRUE, digits = NA, null = "null")))
    }
    if (terra::nlyr(target) != 1L || !isTRUE(horizontal(sources[1L]) == horizontal(template)))
      stop("Source DEMs and output template must use the same projected horizontal CRS.")
    if (!is.null(extent)) target <- terra::crop(target, terra::ext(extent), snap = "out")
    # Resample within one CRS, never warp a compound CRS into a 2D CRS.
    # This preserves the source vertical declaration without applying a height operation.
    terra::crs(target) <- terra::crs(reference)
    if (mixed) {
      # Preserve priority by grouping only consecutive compatible sources. A
      # later tile must never leap ahead of an intervening higher-priority tile.
      new_run <- c(TRUE,vapply(seq_len(length(rasters)-1L),function(i)
        !same_grid(rasters[[i]],rasters[[i+1L]]),logical(1)))
      runs <- split(seq_along(sources),cumsum(new_run))
      te <- as.vector(terra::ext(target))
      intersects <- function(r) {
        e <- as.vector(terra::ext(r))
        e[1]<te[2] && e[2]>te[1] && e[3]<te[4] && e[4]>te[3]
      }
      runs <- Filter(function(indices) any(vapply(rasters[indices],intersects,logical(1))),runs)
      # A noncontributing run must not split an otherwise continuous tile seam.
      # Keep all tiles in each retained run, including those supplying its halo.
      active <- list()
      for(indices in runs) {
        n <- length(active)
        if(n && same_grid(rasters[[tail(active[[n]],1L)]],rasters[[indices[1L]]]))
          active[[n]] <- c(active[[n]],indices) else active[[n+1L]] <- indices
      }
      runs <- active
      if (!length(runs)) stop("No source tile intersects the requested output grid.")
      scratch <- tempfile("mixed-grids-",tmpdir=dirname(filename))
      if (!dir.create(scratch)) stop("Cannot create mixed-grid staging.")
      on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
      rendered <- list(); evidence <- list()
      for (indices in runs) {
        # Use the identical Event template for every run. Cropping the target
        # first changes native bilinear edge weights for partial source coverage.
        path <- file.path(scratch,paste0("run-",length(rendered)+1L,".tif"))
        part <- mosaic_terrain_tiles(sources[indices],path,overlap,
          extent=te,template=template)
        rendered[[length(rendered)+1L]] <- terra::rast(path)
        evidence[[length(evidence)+1L]] <- list(source_indices=indices,
          resolution=terra::res(rasters[[indices[1L]]]),
          origin=terra::origin(rasters[[indices[1L]]]),halo=part$halo)
      }
      if (!length(rendered)) stop("No source tile intersects the requested output grid.")
      options <- list(datatype="FLT4S",gdal=c("COMPRESS=DEFLATE","PREDICTOR=3","TILED=YES","BIGTIFF=IF_SAFER"))
      complete <- FALSE
      on.exit(if(!complete) unlink(filename),add=TRUE)
      if(length(rendered)==1L) terra::writeRaster(rendered[[1L]],filename,wopt=options) else
        terra::merge(terra::sprc(rendered),first=overlap=="first",na.rm=TRUE,algo=1,
          resample=FALSE,filename=filename,wopt=options)
      reopened <- terra::rast(filename)
      if (!terra::compareGeom(target,reopened,stopOnError=FALSE) ||
          !identical(terra::datatype(reopened),"FLT4S") ||
          !identical(terra::units(reference),terra::units(reopened)))
        stop("Mixed-grid output did not preserve the Event grid, source CRS and elevation units.")
      complete <- TRUE
      return(list(path=normalizePath(filename,winslash="/"),sources=sources,overlap=overlap,
        requested_extent=extent,template=normalizePath(template,winslash="/"),
        method="merge consecutive source-grid tiles; bilinear resample; first/last-valid merge on Event grid",
        resampling="bilinear",mixed_source_grids=TRUE,source_grid_runs=evidence,
        source_resolutions=unique(lapply(rasters,terra::res)),
        crs=terra::crs(reopened),resolution=terra::res(reopened),extent=as.vector(terra::ext(reopened)),
        dimensions=dim(reopened),datatype=terra::datatype(reopened),units=terra::units(reopened),
        terra_version=as.character(utils::packageVersion("terra")),scientific_acceptance=FALSE))
    }
    halo <- 2 * pmax(terra::res(reference), terra::res(target))
    window <- as.vector(terra::ext(target)) + c(-halo[1], halo[1], -halo[2], halo[2])
    joined <- tempfile("source-grid-", tmpdir = dirname(filename), fileext = ".tif")
    on.exit(unlink(joined), add = TRUE)
    mosaic_terrain_tiles(sources, joined, overlap, extent = window)
    x <- terra::rast(joined)
    complete <- FALSE
    on.exit(if (!complete) unlink(filename), add = TRUE)
    terra::units(target) <- terra::units(x)
    terra::resample(x, target, method = "bilinear", filename = filename,
      wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3",
        "TILED=YES", "BIGTIFF=IF_SAFER")))
    reopened <- terra::rast(filename)
    if (!terra::compareGeom(target, reopened, stopOnError = FALSE) ||
        !identical(terra::datatype(reopened), "FLT4S") ||
        !identical(terra::units(x), terra::units(reopened)) ||
        !isTRUE(sf::st_crs(terra::crs(x)) == sf::st_crs(terra::crs(reopened))))
      stop("Resampled output did not preserve the target grid, source CRS and elevation units.")
    complete <- TRUE
    return(list(path = normalizePath(filename, winslash = "/"), sources = sources,
      overlap = overlap, requested_extent = extent, template = normalizePath(template, winslash = "/"),
      method = "terra::merge then terra::resample (bilinear); no elevation conversion",
      resampling = "bilinear", mixed_source_grids = FALSE,
      source_resolution = terra::res(reference), halo = halo,
      crs = terra::crs(reopened), resolution = terra::res(reopened),
      extent = as.vector(terra::ext(reopened)), dimensions = dim(reopened),
      datatype = terra::datatype(reopened), units = terra::units(reopened),
      terra_version = as.character(utils::packageVersion("terra")), scientific_acceptance = FALSE))
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
