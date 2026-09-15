#' Clip and mask terrain while recording the executed operation
#'
#' Creates a new delivery folder containing terrain.tif, the supplied AOI geometry,
#' an intake manifest and a machine-recorded execution receipt. Source files and
#' saved Study Area contexts are never changed. This is actual processing, not an
#' analyst's retrospective preparation account.
#'
#' @param dem Existing self-contained, single-band GeoTIFF with a projected,
#'   two-dimensional CRS. Auxiliary files, compound/3D reference systems, rotated
#'   grids and categorical rasters are not qualified by this first slice.
#' @param aoi Nonempty valid polygon sf in the same CRS as dem. Every feature is
#'   included; attributes are not retained. The AOI must lie within the DEM's
#'   rectangular extent. No automatic repair, buffering or reprojection occurs.
#' @param output_dir New folder in an existing directory. Existing destinations
#'   are refused. Incomplete runs remain available for diagnosis, not reuse.
#' @param rationale Nonempty explanation of why this AOI was chosen. This supplied
#'   judgment is separate from automatically observed execution evidence.
#' @param touches Logical. TRUE (default) retains cells touched by the polygon;
#'   FALSE uses cell centers. Bounds snap outward to existing cell boundaries.
#' @param report Logical; also render a short HTML report, default FALSE. Report
#'   failure leaves the completed terrain and receipt available for read-only retry.
#' @return Invisible list with terrain, aoi, manifest, execution and report paths.
#' @details Uses terra crop/mask without resampling or changing elevation units.
#'   Retained values and NoData are checked blockwise after writing Float64 terrain.
#'   NoData is an intentional mask, not a missing-data percentage or quality score.
#'   Unknown vertical references remain unknown. Known compound/3D CRS are refused
#'   rather than silently flattened. Success verifies this local operation only,
#'   not original lidar lineage, AOI suitability or change-over-time comparability.
#'   A started.json receipt is written before computation; execution.json is written
#'   only on success. Caught processing errors write failure.json; abrupt termination
#'   may leave only started.json. No script or expression is read from these files.
#' @seealso [terrain_clip_report()], [write_terrain_manifest()]
#' @md
#' @export
clip_terrain_to_aoi <- function(
  dem,
  aoi,
  output_dir,
  rationale,
  touches = TRUE,
  report = FALSE
) {
  rationale <- .fg_required_text(rationale, "rationale")
  for (value in list(touches, report)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .fg_abort("touches and report must be single nonmissing logical values.")
    }
  }
  output_dir <- .fg_required_text(output_dir, "output_dir")
  if (!dir.exists(dirname(output_dir)) || file.exists(output_dir)) {
    .fg_abort("Supply a new output folder in an existing directory.")
  }
  parent <- normalizePath(dirname(output_dir), winslash = "/", mustWork = TRUE)
  output_dir <- .fg_manifest_path(parent, basename(output_dir))
  observation <- inspect_terrain_vertical_reference(dem)
  dem <- observation$path
  companions <- .fg_manifest_companions(dirname(dem), basename(dem))
  if (length(companions) || file.exists(paste0(dem, ".msk"))) {
    .fg_abort(
      "This clip slice requires a self-contained GeoTIFF without auxiliary files; resolve their meaning before processing."
    )
  }
  reference <- observation$internal_compound
  if (
    !identical(reference$projjson$type, "ProjectedCRS") ||
      length(reference$projjson$coordinate_system$axis) != 2L ||
      reference$status == "VERTICAL_CRS_EXPOSED"
  ) {
    .fg_abort(
      "This clip slice requires a projected 2D CRS; compound/3D preservation is not yet qualified."
    )
  }
  if (!identical(observation$default_reader$wkt, reference$wkt)) {
    .fg_abort(
      "Default and internal CRS observations differ; resolve the reader boundary before clipping."
    )
  }
  raster <- terra::rast(dem, opts = "GEOREF_SOURCES=INTERNAL")
  if (terra::is.rotated(raster) || any(terra::is.factor(raster))) {
    .fg_abort("Rotated grids and categorical rasters are not supported.")
  }
  if (
    !terra::datatype(raster) %in%
      c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")
  ) {
    .fg_abort(
      "Unsupported DEM pixel type; exact Float64 representation is required."
    )
  }
  .fg_terrain_polygon(aoi)
  if (
    !nrow(aoi) || !isTRUE(sf::st_crs(aoi) == sf::st_crs(terra::crs(raster)))
  ) {
    .fg_abort(
      "Supply a nonempty AOI in the DEM CRS; no reprojection is performed."
    )
  }
  if (
    any(
      !vapply(
        sf::st_geometry(aoi),
        function(g) identical(class(g)[1L], "XY"),
        logical(1)
      )
    )
  ) {
    .fg_abort("AOI geometry must be two-dimensional XY.")
  }
  box <- sf::st_bbox(aoi)
  extent <- as.vector(terra::ext(raster))
  if (
    box[["xmin"]] < extent[1] ||
      box[["xmax"]] > extent[2] ||
      box[["ymin"]] < extent[3] ||
      box[["ymax"]] > extent[4]
  ) {
    .fg_abort(
      "AOI extends beyond the DEM extent; choose the matching source or an explicitly smaller AOI."
    )
  }
  aoi <- sf::st_sf(geometry = sf::st_geometry(aoi))
  source_grid <- .fg_manifest_raster(dem)
  now <- function() {
    format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  }
  receipt <- list(
    schema = "FLUVGEO_TERRAIN_CLIP_RUN_1",
    status = "STARTED",
    operation = "CLIP_MASK_TERRAIN",
    started_at = now(),
    rationale = rationale,
    input = list(
      filename = basename(dem),
      sha256 = observation$sha256,
      bytes = unname(file.info(dem)$size),
      grid = source_grid,
      reference = reference
    ),
    parameters = list(
      snap = "out",
      touches = touches,
      resampling = "none",
      output_datatype = "FLT8S",
      compression = "DEFLATE"
    ),
    software = list(
      fluvgeo = as.character(utils::packageVersion("fluvgeo")),
      R = as.character(getRversion()),
      terra = as.character(utils::packageVersion("terra")),
      sf = as.character(utils::packageVersion("sf")),
      terra_geospatial = as.list(terra::gdal(lib = "all")),
      sf_geospatial = as.list(sf::sf_extSoftVersion())
    )
  )
  if (!dir.create(output_dir, showWarnings = FALSE)) {
    .fg_abort("Could not exclusively create the new output folder.")
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  save_json <- function(x, filename) {
    jsonlite::write_json(
      x,
      file.path(output_dir, filename),
      auto_unbox = TRUE,
      pretty = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )
  }
  result <- tryCatch(
    {
      sf::st_write(
        aoi,
        file.path(output_dir, "aoi.gpkg"),
        layer = "aoi",
        quiet = TRUE
      )
      receipt$aoi <- list(
        path = "aoi.gpkg",
        layer = "aoi",
        features = nrow(aoi),
        sha256 = .fg_file_sha256(file.path(output_dir, "aoi.gpkg"))
      )
      save_json(receipt, "started.json")
      retained <- sf::st_read(
        file.path(output_dir, "aoi.gpkg"),
        layer = "aoi",
        quiet = TRUE
      )
      if (
        !identical(
          sf::st_as_binary(sf::st_geometry(aoi)),
          sf::st_as_binary(sf::st_geometry(retained))
        ) ||
          !isTRUE(sf::st_crs(aoi) == sf::st_crs(retained))
      ) {
        .fg_abort("AOI geometry or CRS did not survive storage.")
      }
      cropped <- terra::crop(raster, terra::vect(retained), snap = "out")
      expected <- terra::mask(cropped, terra::vect(retained), touches = touches)
      terrain <- file.path(output_dir, "terrain.tif")
      terra::writeRaster(
        expected,
        terrain,
        overwrite = FALSE,
        datatype = "FLT8S",
        gdal = c("COMPRESS=DEFLATE", "GEOTIFF_VERSION=1.1")
      )
      written <- terra::rast(terrain)
      .fg_clip_verify_values(expected, written)
      after_reference <- inspect_terrain_vertical_reference(
        terrain
      )$internal_compound
      if (
        !isTRUE(sf::st_crs(reference$wkt) == sf::st_crs(after_reference$wkt)) ||
          !identical(reference$band_unit, after_reference$band_unit)
      ) {
        .fg_abort("CRS or elevation-band unit did not survive clipping.")
      }
      if (
        !identical(observation$sha256, .fg_file_sha256(dem)) ||
          !identical(
            receipt$aoi$sha256,
            .fg_file_sha256(file.path(output_dir, "aoi.gpkg"))
          ) ||
          length(.fg_manifest_companions(dirname(dem), basename(dem))) ||
          file.exists(paste0(dem, ".msk"))
      ) {
        .fg_abort(
          "Source or retained AOI changed during clipping; do not use the partial output."
        )
      }
      manifest <- write_terrain_manifest(
        output_dir,
        data.frame(
          artifact_id = c("clipped-dem", "clip-aoi"),
          path = c("terrain.tif", "aoi.gpkg"),
          role = c("AOI-masked DEM", "Explicit clipping AOI")
        ),
        "Terrain clipping run"
      )
      receipt$status <- "SUCCEEDED"
      receipt$completed_at <- now()
      receipt$output <- list(
        path = "terrain.tif",
        sha256 = .fg_file_sha256(terrain),
        grid = .fg_manifest_raster(terrain),
        reference = after_reference
      )
      receipt$manifest <- list(
        path = basename(manifest),
        sha256 = .fg_file_sha256(manifest)
      )
      receipt$checks <- list(
        retained_values_and_nodata = TRUE,
        grid_preserved = TRUE,
        crs_and_band_unit_preserved = TRUE,
        aoi_geometry_preserved = TRUE,
        source_unchanged = TRUE
      )
      save_json(receipt, "execution.json")
      list(
        terrain = terrain,
        aoi = file.path(output_dir, "aoi.gpkg"),
        manifest = manifest,
        execution = file.path(output_dir, "execution.json"),
        report = NULL
      )
    },
    error = function(e) {
      receipt$status <- "FAILED"
      receipt$failed_at <- now()
      receipt$error <- conditionMessage(e)
      try(save_json(receipt, "failure.json"), silent = TRUE)
      stop(e)
    }
  )
  if (report) {
    result$report <- terrain_clip_report(
      result$execution,
      file.path(output_dir, "terrain-clip.html")
    )
  }
  invisible(result)
}

.fg_clip_verify_values <- function(expected, written) {
  if (!isTRUE(terra::compareGeom(expected, written, stopOnError = FALSE))) {
    .fg_abort("Output grid differs from the intended source-grid subset.")
  }
  terra::readStart(expected)
  on.exit(terra::readStop(expected), add = TRUE)
  terra::readStart(written)
  on.exit(terra::readStop(written), add = TRUE)
  # Bound verification memory independently of total raster size.
  rows <- max(1L, floor(1e6 / ncol(expected)))
  for (row in seq.int(1L, nrow(expected), by = rows)) {
    n <- min(rows, nrow(expected) - row + 1L)
    a <- terra::readValues(expected, row = row, nrows = n)
    b <- terra::readValues(written, row = row, nrows = n)
    if (!identical(is.na(a), is.na(b)) || !all(a[!is.na(a)] == b[!is.na(b)])) {
      .fg_abort(
        "Retained values or NoData changed during output serialization."
      )
    }
  }
  invisible(TRUE)
}

#' Review a completed terrain clipping run
#'
#' Checks the local output fingerprints before rendering the recorded operation.
#' This is an integrity check of a local receipt, not independent proof of its
#' authenticity or original source lineage. Source filename/hash identify the
#' input; the original source is not copied, located or fetched by this report.
#' @param execution Existing execution.json from clip_terrain_to_aoi().
#' @param output_file New HTML file in an existing directory.
#' @return Normalized report path invisibly. Requires gt, knitr and Pandoc.
#' @export
terrain_clip_report <- function(execution, output_file) {
  execution <- .fg_required_text(execution, "execution")
  if (
    !file.exists(execution) ||
      dir.exists(execution) ||
      file.info(execution)$size > 1e7
  ) {
    .fg_abort("Supply an existing execution receipt smaller than 10 MB.")
  }
  x <- jsonlite::read_json(execution, simplifyVector = FALSE)
  if (
    !identical(x$schema, "FLUVGEO_TERRAIN_CLIP_RUN_1") ||
      !identical(x$status, "SUCCEEDED") ||
      !identical(x$operation, "CLIP_MASK_TERRAIN")
  ) {
    .fg_abort("Supply a completed terrain-clipping execution receipt.")
  }
  root <- normalizePath(dirname(execution), winslash = "/", mustWork = TRUE)
  for (asset in list(x$aoi, x$output, x$manifest)) {
    path <- .fg_manifest_path(root, asset$path)
    if (!file.exists(path) || !identical(asset$sha256, .fg_file_sha256(path))) {
      .fg_abort(
        "A clipping output is missing or changed; review integrity before reporting success."
      )
    }
  }
  output_file <- .fg_required_text(output_file, "output_file")
  if (
    !grepl("\\.html$", output_file, ignore.case = TRUE) ||
      !dir.exists(dirname(output_file)) ||
      file.exists(output_file)
  ) {
    .fg_abort("Supply a new .html path in an existing directory.")
  }
  if (
    !requireNamespace("knitr", quietly = TRUE) || !rmarkdown::pandoc_available()
  ) {
    .fg_abort(
      "Rendering requires knitr and Pandoc; the completed clipping run is preserved."
    )
  }
  output_file <- file.path(
    normalizePath(dirname(output_file), winslash = "/"),
    basename(output_file)
  )
  stage <- tempfile(
    "clip-report-",
    tmpdir = dirname(output_file),
    fileext = ".html"
  )
  on.exit(unlink(stage), add = TRUE)
  rmarkdown::render(
    system.file("reports", "terrain_clip_report.Rmd", package = "fluvgeo"),
    output_file = stage,
    intermediates_dir = tempdir(),
    params = list(
      report = x,
      terrain = .fg_manifest_path(root, x$output$path),
      aoi = .fg_manifest_path(root, x$aoi$path)
    ),
    envir = .fg_report_environment(),
    quiet = TRUE
  )
  if (!isTRUE(suppressWarnings(file.link(stage, output_file)))) {
    .fg_abort(
      "Could not publish report without replacement; use a hard-link-capable filesystem."
    )
  }
  invisible(output_file)
}
