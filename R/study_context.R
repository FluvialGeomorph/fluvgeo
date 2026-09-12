#' Save supplied Study Area report context
#'
#' Creates a new context GeoPackage, separate from the network bundle. Supplied
#' identities, optional polygon AOIs, inventory, forensic interpretations and
#' notes are retained without reconciliation or acceptance. External network and
#' terrain-manifest references are relative to the context file's directory and
#' pinned by SHA-256. Move the whole folder, not just this GeoPackage.
#'
#' @param dsn New local .gpkg path in an existing directory.
#' @param study_area,streams,reaches,survey_events,reconstruction Context tables
#'   accepted by terrain_development_summary(). Only documented context columns
#'   are supported; unsupported columns/types fail rather than being dropped.
#'   Survey date components must be integer columns. Optional source_dataset and
#'   availability_notes are text. AOIs must be XY POLYGON or MULTIPOLYGON sf.
#' @param network Optional root-relative path to an existing network GeoPackage.
#' @param folder_manifest Optional root-relative path to a terrain intake JSON.
#'   Explicit event links in that manifest select external GeoTIFF grids; direct
#'   SpatRaster objects and an independent overview DEM are not persisted.
#' @param terrain_notes,analyst_notes Optional scalar text, as in the summary.
#' @return Normalized context path invisibly. New-file publication requires hard
#'   links. Invalid hierarchy fails; missing AOIs and incomplete evidence remain
#'   report findings. This is not a complete FGDB project or event-folder binding.
#' @export
write_study_context <- function(dsn, study_area = NULL, streams = NULL,
    reaches = NULL, survey_events = NULL, reconstruction = NULL, network = NULL,
    folder_manifest = NULL, terrain_notes = NA_character_, analyst_notes = NA_character_) {
  dsn <- .fg_network_dsn(dsn)
  if (file.exists(dsn)) .fg_abort("Context destination already exists.")
  tabs <- list(study_area = study_area, streams = streams, reaches = reaches,
    survey_events = survey_events, reconstruction = reconstruction)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  .fg_study_tables_check(tabs)
  metadata <- data.frame(schema = "FLUVGEO_STUDY_CONTEXT_1",
    terrain_notes = .fg_optional_text(terrain_notes, "terrain_notes"),
    analyst_notes = .fg_optional_text(analyst_notes, "analyst_notes"),
    network = NA_character_, network_sha256 = NA_character_,
    folder_manifest = NA_character_, folder_manifest_sha256 = NA_character_)
  for (nm in c("network", "folder_manifest")) {
    relative <- if (nm == "network") network else folder_manifest
    if (!is.null(relative)) {
      path <- .fg_manifest_path(dirname(dsn), relative)
      if (!file.exists(path) || dir.exists(path)) .fg_abort("Linked context file is missing.")
      metadata[[nm]] <- relative
      metadata[[paste0(nm, "_sha256")]] <- .fg_file_sha256(path)
    }
  }
  args <- .fg_study_arguments(tabs, metadata, dirname(dsn))
  do.call(terrain_development_summary, args) # Reuse existing identity/evidence checks.
  catalog <- data.frame(table_name = as.character(names(tabs)), geometry_column = vapply(tabs,
    function(x) if (inherits(x, "sf")) attr(x, "sf_column") else "", character(1)))
  stage <- tempfile("study-context-", tmpdir = dirname(dsn), fileext = ".gpkg")
  on.exit(unlink(stage), add = TRUE)
  sf::st_write(metadata, stage, layer = "fluvgeo_study_context", quiet = TRUE)
  sf::st_write(catalog, stage, layer = "fluvgeo_study_tables", quiet = TRUE)
  for (nm in names(tabs)) sf::st_write(tabs[[nm]], stage, layer = nm, quiet = TRUE)
  restored <- read_study_context(stage)
  for (nm in names(tabs)) {
    a <- tabs[[nm]]; b <- restored[[nm]]
    if (inherits(a, "sf")) {
      if (!isTRUE(sf::st_crs(a) == sf::st_crs(b)) ||
          !identical(sf::st_as_binary(sf::st_geometry(a)), sf::st_as_binary(sf::st_geometry(b))))
        .fg_abort("Context geometry round-trip mismatch.")
      a <- sf::st_drop_geometry(a); b <- sf::st_drop_geometry(b)
    }
    if (!setequal(names(a), names(b)) || nrow(a) != nrow(b) ||
        !all(vapply(names(a), function(k) identical(unname(a[[k]]), unname(b[[k]])), logical(1))))
      .fg_abort("Context attribute round-trip mismatch.")
  }
  if (!isTRUE(suppressWarnings(file.link(stage, dsn))))
    .fg_abort("Could not publish context without replacement; use a hard-link-capable filesystem.")
  invisible(dsn)
}

#' Reopen a saved Study Area context
#'
#' Reads supplied records and resolves pinned local references without changing
#' sources. Missing/changed network or manifest files fail explicitly. Missing or
#' changed terrain assets inside a valid manifest remain fresh report findings;
#' no replacement grid or hierarchy is inferred. Structural/parentage validation
#' reuses terrain_development_summary().
#'
#' @param dsn Context GeoPackage from write_study_context().
#' @param terrain_references Logical; opt in to fresh reference inspection of
#'   explicitly selected event DEMs in read_study_context_summary(). Defaults to
#'   FALSE. Blocked selections remain findings and are not inspected as substitutes.
#' @param analysis_reference Optional attributed project choices accepted by
#'   terrain_reference_review(). Requires terrain_references = TRUE. These are
#'   report inputs only, not saved context or inferred from manifest assertions.
#' @return Named arguments for terrain_development_summary(), with resolved local
#'   network/manifest paths. Absent context stays absent. No cached validation or
#'   raster object is persisted. read_study_context_summary() returns the freshly
#'   computed report summary instead.
#' @export
read_study_context <- function(dsn) {
  args <- .fg_read_study_context(dsn)
  do.call(terrain_development_summary, args)
  args
}

.fg_read_study_context <- function(dsn) {
  dsn <- .fg_network_dsn(dsn)
  if (!file.exists(dsn) || dir.exists(dsn)) .fg_abort("Context GeoPackage does not exist.")
  layers <- sf::st_layers(dsn)$name
  required <- c("fluvgeo_study_context", "fluvgeo_study_tables")
  if (!all(required %in% layers)) .fg_abort("Missing Study Area context binding.")
  metadata <- sf::st_read(dsn, layer = required[1], quiet = TRUE)
  fields <- c("schema", "terrain_notes", "analyst_notes", "network", "network_sha256",
    "folder_manifest", "folder_manifest_sha256")
  if (inherits(metadata, "sf") || nrow(metadata) != 1L || !setequal(names(metadata), fields) ||
      !all(vapply(metadata, is.character, logical(1))) ||
      !identical(metadata$schema, "FLUVGEO_STUDY_CONTEXT_1"))
    .fg_abort("Unsupported or malformed Study Area context metadata.")
  catalog <- sf::st_read(dsn, layer = required[2], quiet = TRUE)
  if (inherits(catalog, "sf") || !setequal(names(catalog), c("table_name", "geometry_column")) ||
      !all(vapply(catalog, is.character, logical(1))) || anyNA(catalog) ||
      anyDuplicated(catalog$table_name) || any(!catalog$table_name %in% names(.fg_study_columns())) ||
      !setequal(layers, c(required, catalog$table_name)))
    .fg_abort("Malformed Study Area context table catalog.")
  tabs <- list()
  for (i in seq_len(nrow(catalog))) {
    nm <- catalog$table_name[i]; geom <- catalog$geometry_column[i]
    x <- sf::st_read(dsn, layer = nm, quiet = TRUE, stringsAsFactors = FALSE)
    if (inherits(x, "sf") != nzchar(geom)) .fg_abort("Context geometry catalog mismatch.")
    if (nzchar(geom)) {
      old <- attr(x, "sf_column")
      if (geom != old && geom %in% names(x)) .fg_abort("Duplicate context geometry column.")
      names(x)[names(x) == old] <- geom
      sf::st_geometry(x) <- geom
    }
    tabs[[nm]] <- x
  }
  .fg_study_tables_check(tabs)
  args <- .fg_study_arguments(tabs, metadata, dirname(dsn))
  args
}

#' @rdname read_study_context
#' @export
read_study_context_summary <- function(dsn, terrain_references = FALSE,
    analysis_reference = NULL) {
  if (!is.logical(terrain_references) || length(terrain_references) != 1L || is.na(terrain_references))
    .fg_abort("terrain_references must be TRUE or FALSE.")
  if (!terrain_references && !is.null(analysis_reference))
    .fg_abort("analysis_reference requires terrain_references = TRUE.")
  args <- .fg_read_study_context(dsn)
  manifest_hash <- if (terrain_references && !is.null(args$folder_manifest))
    .fg_file_sha256(args$folder_manifest) else NULL
  summary <- do.call(terrain_development_summary, args)
  if (terrain_references) summary$terrain_review <-
    .fg_study_terrain_references(summary, args$folder_manifest, analysis_reference)
  if (!is.null(manifest_hash) && !identical(manifest_hash, .fg_file_sha256(args$folder_manifest)))
    .fg_abort("Manifest changed during reference review; retry with a stable saved context.")
  summary
}

.fg_study_columns <- function() list(
  study_area = c("study_area_id", "study_area_name"),
  streams = c("stream_id", "study_area_id", "stream_name"),
  reaches = c("reach_id", "stream_id", "reach_name"),
  survey_events = c("survey_event_id", "reach_id", "survey_year", "survey_month",
    "survey_day", "source_dataset", "availability_notes"),
  reconstruction = c("case_id", "source_ref", "proposed_structure", "evidence",
    "status", "analyst", "decision_notes"))

.fg_study_tables_check <- function(tabs) {
  for (nm in names(tabs)) {
    x <- tabs[[nm]]
    if (!is.data.frame(x) || anyDuplicated(names(x))) .fg_abort("Context requires data frames with unique fields.")
    if (inherits(x, "sf")) {
      if (!nm %in% c("study_area", "streams", "reaches") || is.na(sf::st_crs(x)) ||
          !class(sf::st_geometry(x))[1] %in% c("sfc_POLYGON", "sfc_MULTIPOLYGON") ||
          !all(vapply(sf::st_geometry(x), inherits, logical(1), "XY")))
        .fg_abort("Context AOIs require CRS-defined XY POLYGON/MULTIPOLYGON geometry.")
      x <- sf::st_drop_geometry(x)
    }
    if (!all(names(x) %in% .fg_study_columns()[[nm]])) .fg_abort("Unsupported context columns; nothing was dropped.")
    for (field in names(x)) {
      v <- x[[field]]
      kind <- if (field %in% c("survey_year", "survey_month", "survey_day")) "integer" else "character"
      if (is.object(v) || !is.null(dim(v)) || typeof(v) != kind)
        .fg_abort(paste("Context field", field, "must be a plain", kind, "column."))
    }
  }
}

.fg_study_arguments <- function(tabs, metadata, root) {
  args <- c(tabs, list(terrain_notes = metadata$terrain_notes, analyst_notes = metadata$analyst_notes))
  for (nm in c("network", "folder_manifest")) {
    relative <- metadata[[nm]]; hash <- metadata[[paste0(nm, "_sha256")]]
    if (is.na(relative) && is.na(hash)) next
    if (is.na(relative) || is.na(hash) || !grepl("^[0-9a-f]{64}$", hash))
      .fg_abort("Malformed context reference hash.")
    path <- .fg_manifest_path(root, relative)
    if (!file.exists(path) || dir.exists(path)) .fg_abort(paste("Linked", nm, "is missing; restore the declared file."))
    if (!identical(.fg_file_sha256(path), hash)) .fg_abort(paste("Linked", nm, "changed; review and save a new context."))
    args[[nm]] <- path
  }
  args
}
