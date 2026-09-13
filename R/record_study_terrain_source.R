#' Record attributed source-use evidence for an inventoried terrain artifact
#'
#' Saves one candidate, recorded-use account, or rejection in a new Study Area
#' context. Source identity is explicitly namespaced; no catalog query, automatic
#' matching, source download, terrain transformation or acceptance occurs.
#'
#' @param dsn Existing saved Study Area context with a terrain manifest.
#' @param output_file New context .gpkg beside dsn. Retain earlier snapshots.
#' @param association_id Caller-supplied local claim ID. Reusing it explicitly
#'   revises status/evidence/description/attribution, not its target or source key.
#' @param artifact_id Exact GeoTIFF artifact ID in the linked manifest. Its saved
#'   SHA-256 pins the derivative; availability remains a separate report finding.
#' @param source_catalog Nonempty source namespace, such as WESM, USIEI, or a
#'   documented local archive namespace. No public catalog membership is required.
#' @param source_record_id Exact identifier in that namespace, not an FG event ID.
#' @param source_snapshot Nonempty locator/reference for the metadata or archive
#'   record consulted. This is retained text, not fetched or checksum-verified.
#' @param source_description Human-readable source/product description.
#' @param status CANDIDATE, RECORDED_USE, or REJECTED. RECORDED_USE is an attributed
#'   account of use, not independently verified lineage or processing execution.
#' @param basis PROJECT_RECORD or OWNER_RECOLLECTION; both require evidence.
#' @param evidence Nonempty account of support, limitations and competing claims.
#' @param analyst Nonempty recorder attribution, not an approval signature.
#' @param source_version Optional source edition/version; NA means unresolved.
#'   Neither a catalog ID nor a version string establishes exact source bytes.
#' @param report_file Optional new HTML report; no fresh reference inspection is
#'   required to show saved source-use evidence.
#' @param report_purpose definition (default), terrain, or staging.
#' @return List with context and report paths. Uses FLUVGEO_STUDY_CONTEXT_3;
#'   schema-1/2 contexts remain readable. Multiple sources per artifact and reuse
#'   across artifacts are supported without merging catalog identities or events.
#'   No in-file revision ledger is created. A later report failure retains the
#'   context for inspection/retry. Source products, recipes, source checksums,
#'   recoverability and cross-catalog reconciliation remain separate work.
#' @export
record_study_terrain_source <- function(dsn, output_file, association_id,
    artifact_id, source_catalog, source_record_id, source_snapshot,
    source_description, status, basis, evidence, analyst,
    source_version = NA_character_, report_file = NULL,
    report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file)) .fg_abort("Save the revised context beside the original.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  report_purpose <- .fg_choice(report_purpose, c("definition", "terrain", "staging"), "report_purpose")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  before <- .fg_file_sha256(dsn)
  args <- read_study_context(dsn)
  if (is.null(args$study_area) || is.null(args$folder_manifest))
    .fg_abort("Source-use records require a Study Area and linked terrain manifest.")
  manifest_hash <- .fg_file_sha256(args$folder_manifest)
  artifact_id <- .fg_required_text(artifact_id, "artifact_id")
  manifest <- jsonlite::read_json(args$folder_manifest)
  i <- match(artifact_id, vapply(manifest$artifacts, `[[`, character(1), "artifact_id"))
  if (is.na(i) || manifest$artifacts[[i]]$observed$format != "GeoTIFF")
    .fg_abort("Select an exact inventoried GeoTIFF artifact ID.")
  row <- data.frame(association_id = .fg_required_text(association_id, "association_id"),
    artifact_id = artifact_id, artifact_sha256 = manifest$artifacts[[i]]$sha256,
    source_catalog = .fg_required_text(source_catalog, "source_catalog"),
    source_record_id = .fg_required_text(source_record_id, "source_record_id"),
    source_snapshot = .fg_required_text(source_snapshot, "source_snapshot"),
    source_description = .fg_required_text(source_description, "source_description"),
    source_version = .fg_optional_text(source_version, "source_version"),
    status = .fg_required_text(status, "status"), basis = .fg_required_text(basis, "basis"),
    evidence = .fg_required_text(evidence, "evidence"), analyst = .fg_required_text(analyst, "analyst"),
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  .fg_terrain_sources_check(row)
  old <- args$terrain_sources
  if (is.null(old)) old <- row[FALSE, ]
  i <- match(row$association_id, old$association_id)
  if (is.na(i)) old <- rbind(old, row) else {
    key <- c("artifact_id", "artifact_sha256", "source_catalog", "source_record_id",
      "source_snapshot", "source_version")
    if (!all(vapply(key, function(k) identical(old[[k]][i], row[[k]]), logical(1))))
      .fg_abort("An existing association cannot be retargeted; retain/reject it and record a new claim ID.")
    fields <- setdiff(names(row), "recorded_at")
    if (all(vapply(fields, function(k) identical(old[[k]][i], row[[k]]), logical(1))))
      .fg_abort("No change supplied; the source-use record is unchanged.")
    old[i, ] <- row
  }
  args$terrain_sources <- old
  .fg_terrain_sources_check(old)
  if (!identical(before, .fg_file_sha256(dsn)) ||
      !identical(manifest_hash, .fg_file_sha256(args$folder_manifest)))
    .fg_abort("Context or manifest changed during recording; retry against stable evidence.")
  .fg_save_study_revision(args, dsn, output_file, report_file, report_purpose)
}

.fg_terrain_source_fields <- function() c("association_id", "artifact_id", "artifact_sha256",
  "source_catalog", "source_record_id", "source_snapshot", "source_description",
  "source_version", "status", "basis", "evidence", "analyst", "recorded_at")

.fg_terrain_sources_check <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  fields <- .fg_terrain_source_fields()
  if (!is.data.frame(x) || inherits(x, "sf") || !nrow(x) ||
      anyDuplicated(names(x)) || !setequal(names(x), fields))
    .fg_abort("terrain_sources requires nonempty records with exactly the supported fields.")
  for (field in fields) {
    v <- x[[field]]
    if (!is.character(v) || is.object(v) || !is.null(dim(v)))
      .fg_abort("terrain_sources fields must be plain character columns.")
    if (field == "source_version") v <- v[!is.na(v)]
    if (length(v)) .fg_required_text(v, field, length(v))
  }
  if (anyDuplicated(x$association_id)) .fg_abort("Source association IDs must be unique.")
  if (any(!grepl("^[0-9a-f]{64}$", x$artifact_sha256))) .fg_abort("Invalid terrain source target SHA-256.")
  if (any(!x$status %in% c("CANDIDATE", "RECORDED_USE", "REJECTED")) ||
      any(!x$basis %in% c("PROJECT_RECORD", "OWNER_RECOLLECTION")))
    .fg_abort("Unsupported source-use status or evidence basis.")
  key <- c("artifact_id", "source_catalog", "source_record_id", "source_snapshot", "source_version")
  if (anyDuplicated(x[key])) .fg_abort("Duplicate source association; explicitly revise the existing claim ID.")
  parsed <- as.POSIXct(strptime(x$recorded_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (anyNA(parsed) || any(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != x$recorded_at))
    .fg_abort("recorded_at must be a valid UTC timestamp in YYYY-MM-DDTHH:MM:SSZ form.")
  invisible(NULL)
}

.fg_terrain_sources_bind <- function(x, manifest, study_area) {
  if (is.null(x)) return(invisible(NULL))
  .fg_terrain_sources_check(x)
  if (is.null(manifest) || is.null(study_area))
    .fg_abort("Source-use records require a Study Area and linked terrain manifest.")
  # The summary already validates/inspects this manifest. Bind to its saved bytes,
  # not whatever currently occupies the path; missing/changed files stay findings.
  records <- jsonlite::read_json(manifest)$artifacts
  i <- match(x$artifact_id, vapply(records, `[[`, character(1), "artifact_id"))
  if (anyNA(i)) .fg_abort("Source-use record references an artifact absent from the manifest.")
  hashes <- vapply(records[i], `[[`, character(1), "sha256")
  formats <- vapply(records[i], function(a) a$observed$format, character(1))
  if (any(formats != "GeoTIFF") || !identical(unname(x$artifact_sha256), unname(hashes)))
    .fg_abort("Source-use target differs from the inventoried GeoTIFF fingerprint; no claim was transferred.")
  invisible(NULL)
}
