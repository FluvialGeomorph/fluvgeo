#' Record a portable terrain intake inventory
#'
#' Snapshots explicitly selected files without copying, converting or accepting
#' them. This development binding is an intake inventory, not an FGDB event
#' manifest or proof of complete project delivery. Paths are relative to root;
#' intake and artifact IDs are caller-supplied local labels, not governed UUIDs.
#'
#' @param root Existing intake folder containing the selected files.
#' @param artifacts Data frame with unique character artifact_id, path and role
#'   columns. Paths must identify GeoTIFF or GeoPackage files inside root.
#'   Optional character vertical_reference, vertical_unit and metadata_evidence
#'   columns contain caller assertions; known vertical metadata requires evidence.
#'   Missing vertical metadata remains unknown, never inferred from filenames.
#' @param intake_id Nonempty local intake-case label; no hierarchy is inferred.
#' @param filename New JSON filename directly within root; never overwritten.
#' @param event_links Optional data frame with artifact_id, survey_event_id,
#'   purpose, evidence, analyst (character) and use_for_report (logical).
#'   Links are caller assertions, not reconciled identities or acceptance.
#'   At most one GeoTIFF per event may be selected for the report grid view.
#'   Supplying links writes intake schema 2; omission retains schema 1.
#' @return Manifest path invisibly. Publication requires hard-link support.
#' @export
write_terrain_manifest <- function(root, artifacts, intake_id,
    filename = "terrain-manifest.json", event_links = NULL) {
  root <- .fg_manifest_root(root)
  intake_id <- .fg_required_text(intake_id, "intake_id")
  filename <- .fg_required_text(filename, "filename")
  if (basename(filename) != filename || !grepl("^[^:/\\\\]+\\.json$", filename))
    .fg_abort("Supply a simple new JSON filename in root.")
  destination <- .fg_manifest_path(root, filename)
  if (file.exists(destination)) .fg_abort("Manifest destination already exists.")
  .fg_require_table(artifacts, c("artifact_id", "path", "role"), "artifacts")
  if (!nrow(artifacts)) .fg_abort("Select at least one artifact.")
  for (field in c("artifact_id", "path", "role"))
    .fg_required_text(artifacts[[field]], field, nrow(artifacts))
  if (anyDuplicated(artifacts$artifact_id) || anyDuplicated(tolower(artifacts$path)))
    .fg_abort("Artifact IDs and paths must be unique.")
  for (field in c("vertical_reference", "vertical_unit", "metadata_evidence")) {
    if (!field %in% names(artifacts)) artifacts[[field]] <- rep(NA_character_, nrow(artifacts))
    if (!is.character(artifacts[[field]])) .fg_abort("Metadata columns must be character.")
    artifacts[[field]][!is.na(artifacts[[field]]) & !nzchar(trimws(artifacts[[field]]))] <- NA_character_
  }
  known <- !is.na(artifacts$vertical_reference) | !is.na(artifacts$vertical_unit)
  if (any(known & is.na(artifacts$metadata_evidence)))
    .fg_abort("Known vertical metadata requires metadata_evidence.")
  records <- lapply(seq_len(nrow(artifacts)), function(i) {
    x <- as.list(artifacts[i, c("artifact_id", "path", "role", "vertical_reference",
                              "vertical_unit", "metadata_evidence"), drop = FALSE])
    path <- .fg_manifest_path(root, x$path)
    if (!file.exists(path) || dir.exists(path)) .fg_abort("Selected artifact is missing or not a file.")
    x$sha256 <- .fg_file_sha256(path)
    x$bytes <- unname(file.info(path)$size)
    x$observed <- .fg_manifest_observe(path)
    x$companions <- .fg_manifest_companions(root, x$path)
    if (!identical(x$sha256, .fg_file_sha256(path))) .fg_abort("Artifact changed during inventory.")
    x
  })
  links <- .fg_terrain_event_links(event_links, artifacts$artifact_id,
    vapply(records, function(a) a$observed$format, character(1)))
  manifest <- list(schema = if (is.null(event_links)) "FLUVGEO_TERRAIN_INTAKE_1" else "FLUVGEO_TERRAIN_INTAKE_2", intake_id = intake_id,
    created_at = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    software = list(fluvgeo = as.character(utils::packageVersion("fluvgeo")),
                    terra = as.character(utils::packageVersion("terra")),
                    gdal = terra::gdal()), artifacts = records)
  if (!is.null(event_links)) manifest$event_links <- links
  stage <- tempfile("terrain-manifest-", tmpdir = root, fileext = ".json")
  on.exit(unlink(stage), add = TRUE)
  jsonlite::write_json(manifest, stage, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
  inspect_terrain_folder(stage) # Structural/read-back verification; review findings are allowed.
  if (!isTRUE(suppressWarnings(file.link(stage, destination))))
    .fg_abort("Could not publish manifest without replacement; use a hard-link-capable filesystem.")
  invisible(destination)
}

#' Inspect files referenced by a terrain intake manifest
#'
#' Resolves files relative to the manifest's current directory and checks hashes,
#' raster metadata and known companion files. It never repairs data or changes
#' the saved manifest. Successful integrity checks are not scientific acceptance.
#' Undeclared assets, hierarchy, coverage, source-to-copy numerical equivalence,
#' and shared assets outside this intake root are not qualified by this slice.
#'
#' @param manifest Path to a FLUVGEO_TERRAIN_INTAKE_1 or _2 JSON manifest.
#' @return List with schema, intake_id, artifacts (current file status), and
#'   assessment (structured findings for reports and future clients). Schema-2
#'   inspection also returns validated event_links, without resolving event
#'   identities against a registry or selecting scientific inputs.
#' @export
inspect_terrain_folder <- function(manifest) {
  manifest <- .fg_required_text(manifest, "manifest")
  if (!file.exists(manifest) || dir.exists(manifest) || file.info(manifest)$size > 1e7)
    .fg_abort("Supply an existing manifest smaller than 10 MB.")
  manifest <- normalizePath(manifest, winslash = "/", mustWork = TRUE)
  root <- dirname(manifest)
  x <- jsonlite::read_json(manifest, simplifyVector = FALSE)
  if (!is.character(x$schema) || length(x$schema) != 1L ||
      !x$schema %in% c("FLUVGEO_TERRAIN_INTAKE_1", "FLUVGEO_TERRAIN_INTAKE_2") ||
      !is.list(x$artifacts) || !length(x$artifacts)) .fg_abort("Unsupported or malformed terrain manifest.")
  .fg_required_text(x$intake_id, "intake_id")
  assessment <- data.frame(code = character(), entity_id = character(), entity_label = character(),
    stage = character(), status = character(), requires_input = logical(), next_action = character())
  add <- function(code, a, status, action, input = FALSE) {
    assessment <<- rbind(assessment, data.frame(code = code, entity_id = a$artifact_id,
      entity_label = paste(a$role, a$path, sep = " | "), stage = "FILE_INTEGRITY",
      status = status, requires_input = input, next_action = action))
  }
  ids <- paths <- character()
  rows <- lapply(x$artifacts, function(a) {
    for (field in c("artifact_id", "path", "role", "sha256")) .fg_required_text(a[[field]], field)
    for (field in c("vertical_reference", "vertical_unit", "metadata_evidence"))
      if (!is.null(a[[field]])) .fg_required_text(a[[field]], field)
    if ((!is.null(a$vertical_reference) || !is.null(a$vertical_unit)) && is.null(a$metadata_evidence))
      .fg_abort("Known vertical metadata requires metadata_evidence.")
    if (!grepl("^[0-9a-f]{64}$", a$sha256) || !is.numeric(a$bytes) || length(a$bytes) != 1L ||
        !is.finite(a$bytes) || a$bytes < 0 || !is.list(a$observed) ||
        !a$observed$format %in% c("GeoTIFF", "GeoPackage") || !is.list(a$companions))
      .fg_abort("Malformed artifact metadata.")
    ids <<- c(ids, a$artifact_id); paths <<- c(paths, tolower(a$path))
    path <- .fg_manifest_path(root, a$path)
    exists <- file.exists(path) && !dir.exists(path)
    hash_ok <- exists && identical(a$sha256, .fg_file_sha256(path)) && file.info(path)$size == a$bytes
    if (!exists) add("FILE_MISSING", a, "BLOCKED", "Restore the declared file from its recorded source; do not substitute by name.")
    else if (!hash_ok) add("FILE_CHANGED", a, "BLOCKED", "Review the changed file against the saved inventory; do not silently update its checksum.")
    current <- NULL
    if (exists) {
      current <- tryCatch(.fg_manifest_observe(path), error = function(e) NULL)
      if (is.null(current)) add("FILE_UNREADABLE", a, "BLOCKED", "Resolve file format or reader failure before processing.")
      else if (!.fg_manifest_same_metadata(a$observed, current))
        add("RASTER_METADATA_CHANGED", a, "BLOCKED", "Review grid, horizontal CRS, type or band-unit differences against the snapshot.")
    }
    companions <- .fg_manifest_companions(root, a$path)
    if (!identical(jsonlite::toJSON(a$companions, auto_unbox = TRUE),
                   jsonlite::toJSON(companions, auto_unbox = TRUE)))
      add("COMPANION_CHANGED", a, "BLOCKED", "Review added, missing or changed auxiliary files; do not silently prefer sidecar metadata.")
    if (!is.null(current) && current$format == "GeoTIFF") {
      if (!nzchar(current$wkt)) add("CRS_UNKNOWN", a, "REVIEW_REQUIRED", "Recover the horizontal CRS from source evidence.", TRUE)
      if (is.null(a$vertical_reference) || is.null(a$vertical_unit))
        add("VERTICAL_REFERENCE_UNKNOWN", a, "REVIEW_REQUIRED", "Confirm elevation units and vertical reference before operations requiring them; filenames are not evidence.", TRUE)
      if (!is.null(a$vertical_unit) && nzchar(current$band_unit) &&
          .fg_manifest_unit(a$vertical_unit) != .fg_manifest_unit(current$band_unit))
        add("VERTICAL_UNIT_CONFLICT", a, "BLOCKED", "Declared elevation units disagree with the raster band unit; resolve from evidence before analysis.", TRUE)
      if (length(companions)) add("SIDECAR_REVIEW", a, "REVIEW_REQUIRED",
        "Auxiliary files are inventoried but not endorsed; compare their meaning with embedded metadata.", TRUE)
      effective <- tryCatch(.fg_manifest_raster(path, internal = FALSE), error = function(e) NULL)
      if (is.null(effective) || !.fg_manifest_same_metadata(current, effective))
        add("SIDECAR_METADATA_CONFLICT", a, "BLOCKED", "Default reader metadata differs from internal GeoTIFF metadata; resolve the conflict explicitly.", TRUE)
    }
    if (hash_ok && !is.null(current)) add("FILE_HASH_VERIFIED", a, "VERIFIED",
      "File matches the snapshot; this does not certify provenance, coverage or scientific readiness.")
    data.frame(artifact_id = a$artifact_id, role = a$role, path = a$path,
      format = a$observed$format, available = exists, hash_verified = hash_ok,
      vertical_reference = if (is.null(a$vertical_reference)) NA_character_ else a$vertical_reference,
      vertical_unit = if (is.null(a$vertical_unit)) NA_character_ else a$vertical_unit)
  })
  if (anyDuplicated(ids) || anyDuplicated(paths)) .fg_abort("Artifact IDs and paths must be unique.")
  result <- list(schema = "FLUVGEO_TERRAIN_INTAKE_REVIEW_1", intake_id = x$intake_id,
       artifacts = do.call(rbind, rows), assessment = assessment)
  if (x$schema == "FLUVGEO_TERRAIN_INTAKE_2") {
    if (!is.list(x$event_links) || !length(x$event_links)) .fg_abort("Schema 2 requires nonempty event_links.")
    links <- lapply(x$event_links, function(link) {
      if (!is.list(link) || any(lengths(link) != 1L)) .fg_abort("Malformed event link.")
      as.data.frame(link, stringsAsFactors = FALSE)
    })
    result$event_links <- .fg_terrain_event_links(do.call(rbind, links),
      result$artifacts$artifact_id, result$artifacts$format)
    result$schema <- "FLUVGEO_TERRAIN_INTAKE_REVIEW_2"
  } else if (!is.null(x$event_links)) .fg_abort("Event links require intake schema 2.")
  result
}

.fg_manifest_root <- function(root) {
  root <- .fg_required_text(root, "root")
  if (!dir.exists(root)) .fg_abort("Intake root must be an existing directory.")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.fg_manifest_path <- function(root, relative) {
  .fg_required_text(relative, "relative path")
  parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
  if (grepl("[:\\\\]", relative) || startsWith(relative, "/") || endsWith(relative, "/") ||
      any(parts %in% c("", ".", "..")) || any(parts != trimws(parts)) || any(grepl("[.]$", parts)))
    .fg_abort("Unsafe relative path in terrain manifest.")
  candidate <- file.path(root, relative)
  ancestor <- candidate
  while (!file.exists(ancestor) && !dir.exists(ancestor)) ancestor <- dirname(ancestor)
  resolved <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows") { resolved <- tolower(resolved); root <- tolower(root) }
  if (resolved != root && !startsWith(resolved, paste0(root, "/")))
    .fg_abort("Resolved artifact escapes intake root.")
  candidate
}

.fg_file_sha256 <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  unclass(as.character(openssl::sha256(con)))
}

.fg_manifest_companions <- function(root, relative) {
  candidates <- sort(unique(c(paste0(relative, c(".aux.xml", ".ovr")),
    paste0(tools::file_path_sans_ext(relative), c(".tfw", ".tifw", ".wld", ".prj")))))
  result <- list()
  for (p in candidates) {
    full <- .fg_manifest_path(root, p)
    if (file.exists(full) && !dir.exists(full)) result[[length(result) + 1L]] <-
      list(path = p, sha256 = .fg_file_sha256(full))
  }
  result
}

.fg_manifest_observe <- function(path) {
  if (grepl("\\.tiff?$", path, ignore.case = TRUE)) return(.fg_manifest_raster(path))
  if (!grepl("\\.gpkg$", path, ignore.case = TRUE)) .fg_abort("Only GeoTIFF and GeoPackage artifacts are supported.")
  layers <- sf::st_layers(path)
  if (layers$driver != "GPKG") .fg_abort("Expected a GeoPackage driver.")
  list(format = "GeoPackage", layers = sort(layers$name))
}

.fg_manifest_raster <- function(path, internal = TRUE) {
  con <- file(path, "rb"); on.exit(close(con))
  signature <- paste(format(readBin(con, "raw", n = 4L)), collapse = "")
  if (!signature %in% c("49492a00", "4d4d002a", "49492b00", "4d4d002b"))
    .fg_abort("Expected a native TIFF, not a renamed virtual raster or other format.")
  r <- terra::rast(path, opts = if (internal) "GEOREF_SOURCES=INTERNAL" else character())
  if (terra::nlyr(r) != 1L) .fg_abort("The intake slice supports single-band terrain only.")
  list(format = "GeoTIFF", wkt = terra::crs(r), dimensions = c(nrow(r), ncol(r)),
       extent = as.vector(terra::ext(r)), resolution = terra::res(r),
       pixel_type = terra::datatype(r), band_unit = terra::units(r))
}

.fg_manifest_same_metadata <- function(a, b) {
  if (!identical(a$format, b$format)) return(FALSE)
  if (a$format == "GeoPackage") return(identical(as.character(unlist(a$layers)), as.character(unlist(b$layers))))
  for (field in c("dimensions", "pixel_type", "band_unit"))
    if (!isTRUE(all.equal(unlist(a[[field]]), unlist(b[[field]]), tolerance = 0, check.attributes = FALSE))) return(FALSE)
  for (field in c("extent", "resolution")) {
    expected <- unlist(a[[field]]); observed <- unlist(b[[field]])
    if (!is.numeric(expected) || length(expected) != length(observed) ||
        !length(expected) || any(!is.finite(expected)) || any(!is.finite(observed))) return(FALSE)
    # JSON/driver representation allowance only, not a resampling tolerance.
    limit <- 32 * .Machine$double.eps * max(1, abs(expected))
    if (max(abs(expected - observed)) > limit) return(FALSE)
  }
  if (!is.character(a$wkt) || !is.character(b$wkt)) return(FALSE)
  if (!nzchar(a$wkt) || !nzchar(b$wkt)) return(identical(a$wkt, b$wkt))
  isTRUE(sf::st_crs(a$wkt) == sf::st_crs(b$wkt))
}

.fg_manifest_unit <- function(unit) {
  unit <- tolower(trimws(unit))
  if (unit %in% c("m", "metre", "metres", "meter", "meters")) return("m")
  if (unit %in% c("ft", "foot", "feet")) return("ft")
  unit # US survey feet and other labels are not silently conflated or converted.
}
