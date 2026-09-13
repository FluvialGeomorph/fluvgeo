#' Retain a supporting file for a terrain source-use account
#'
#' Copies one explicitly selected local metadata or processing-record file into
#' the linked terrain manifest's folder and records its SHA-256 in a new context.
#' Files are never fetched, parsed, executed, or treated as verified lineage.
#'
#' @param dsn Existing Study Area context with a terrain source-use account.
#' @param output_file New context .gpkg beside dsn.
#' @param evidence_id New caller-supplied local evidence ID; records are append-only.
#' @param association_id Existing source-use claim ID to which this evidence applies.
#' @param source_file Existing local file selected by the analyst. Originals are
#'   unchanged. Select only material suitable for the project's access controls;
#'   do not attach credentials, restricted material or unnecessary personal data.
#' @param kind METADATA_SNAPSHOT or PROCESSING_RECORD. A processing record is
#'   supplied documentation, not proof of execution or an executable recipe.
#' @param description Human-readable description of the selected evidence.
#' @param source_reference Original location/reference and known edition/date.
#'   This text is not fetched; the retention timestamp is not an acquisition date.
#' @param qualifications Explicit limitations and relevance to the claim.
#' @param analyst Recorder attribution, not approval.
#' @param report_file Optional new HTML report path.
#' @param report_purpose definition (default), terrain, or staging.
#' @return List of context and report paths. Schema 4 retains relative paths and
#'   fingerprints; move the complete folder. Fresh reports distinguish matching,
#'   missing, changed and unreadable attachments without promoting source use.
#'   A failure after file publication may retain an unreferenced copy; retry reuses
#'   identical bytes. A later report failure retains the context. No multi-file
#'   transaction, digital signature, exact source-product inventory or processing
#'   execution verification is provided. Earlier snapshots are never replaced.
#' @export
retain_study_terrain_evidence <- function(dsn, output_file, evidence_id,
    association_id, source_file, kind, description, source_reference,
    qualifications, analyst, report_file = NULL, report_purpose = "definition") {
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
  if (is.null(args$terrain_sources) || is.null(args$folder_manifest))
    .fg_abort("Retained evidence requires an existing source-use account and terrain manifest.")
  manifest_hash <- .fg_file_sha256(args$folder_manifest)
  source_file <- .fg_required_text(source_file, "source_file")
  if (!file.exists(source_file) || dir.exists(source_file)) .fg_abort("Select an existing local evidence file.")
  source_file <- normalizePath(source_file, winslash = "/", mustWork = TRUE)
  hash <- .fg_file_sha256(source_file)
  extension <- tolower(tools::file_ext(source_file))
  if (!grepl("^[a-z0-9]{1,10}$", extension)) extension <- "bin"
  relative <- paste0("terrain-evidence/", hash, ".", extension)
  row <- data.frame(evidence_id = .fg_required_text(evidence_id, "evidence_id"),
    association_id = .fg_required_text(association_id, "association_id"),
    kind = .fg_required_text(kind, "kind"), path = relative, sha256 = hash,
    original_name = basename(source_file), description = .fg_required_text(description, "description"),
    source_reference = .fg_required_text(source_reference, "source_reference"),
    qualifications = .fg_required_text(qualifications, "qualifications"),
    analyst = .fg_required_text(analyst, "analyst"),
    retained_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  old <- args$terrain_evidence
  if (is.null(old)) old <- row[FALSE, ]
  args$terrain_evidence <- rbind(old, row)
  .fg_terrain_evidence_check(args$terrain_evidence, args$terrain_sources)
  root <- dirname(args$folder_manifest)
  target <- .fg_manifest_path(root, relative)
  if (file.exists(target) && (dir.exists(target) || !identical(.fg_file_sha256(target), hash)))
    .fg_abort("Retained evidence destination differs; no file was replaced.")
  if (!file.exists(target)) {
    if (!dir.exists(dirname(target)) && !dir.create(dirname(target)))
      .fg_abort("Could not create the evidence directory.")
    # Recheck containment after directory creation; refuse an escaping link.
    target <- .fg_manifest_path(root, relative)
    stage <- tempfile("evidence-", tmpdir = dirname(target))
    on.exit(unlink(stage), add = TRUE)
    if (!file.copy(source_file, stage, overwrite = FALSE) ||
        !identical(.fg_file_sha256(stage), hash) || !identical(.fg_file_sha256(source_file), hash))
      .fg_abort("Evidence changed or could not be copied; retry with stable source bytes.")
    if (!isTRUE(suppressWarnings(file.link(stage, target))))
      .fg_abort("Could not publish evidence without replacement; use a hard-link-capable filesystem.")
  }
  if (!identical(.fg_file_sha256(source_file), hash) ||
      !identical(.fg_file_sha256(target), hash) || !identical(before, .fg_file_sha256(dsn)) ||
      !identical(manifest_hash, .fg_file_sha256(args$folder_manifest)))
    .fg_abort("Evidence, context or manifest changed; a retained copy may remain. Retry with stable inputs.")
  .fg_save_study_revision(args, dsn, output_file, report_file, report_purpose)
}

.fg_terrain_evidence_fields <- function() c("evidence_id", "association_id", "kind",
  "path", "sha256", "original_name", "description", "source_reference",
  "qualifications", "analyst", "retained_at")

.fg_terrain_evidence_check <- function(x, sources) {
  if (is.null(x)) return(invisible(NULL))
  fields <- .fg_terrain_evidence_fields()
  if (!is.data.frame(x) || inherits(x, "sf") || !nrow(x) || anyDuplicated(names(x)) ||
      !setequal(names(x), fields)) .fg_abort("terrain_evidence requires exactly the supported nonempty fields.")
  for (field in fields) {
    v <- x[[field]]
    if (!is.character(v) || is.object(v) || !is.null(dim(v)))
      .fg_abort("terrain_evidence fields must be plain character columns.")
    .fg_required_text(v, field, nrow(x))
  }
  if (is.null(sources) || any(!x$association_id %in% sources$association_id))
    .fg_abort("Evidence must reference an existing source-use account.")
  if (anyDuplicated(x$evidence_id)) .fg_abort("Evidence IDs are append-only and must be unique.")
  if (anyDuplicated(x[c("association_id", "kind", "sha256")]))
    .fg_abort("Identical evidence is already retained for this account and kind.")
  if (any(!x$kind %in% c("METADATA_SNAPSHOT", "PROCESSING_RECORD")))
    .fg_abort("Unsupported evidence kind.")
  if (any(!grepl("^[0-9a-f]{64}$", x$sha256)) ||
      any(!grepl("^terrain-evidence/[0-9a-f]{64}\\.[a-z0-9]{1,10}$", x$path)) ||
      any(substr(basename(x$path), 1L, 64L) != x$sha256))
    .fg_abort("Invalid retained evidence path or SHA-256.")
  parsed <- as.POSIXct(strptime(x$retained_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (anyNA(parsed) || any(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != x$retained_at))
    .fg_abort("retained_at must be a valid UTC timestamp.")
  invisible(NULL)
}

.fg_terrain_evidence_inspect <- function(x, sources, manifest) {
  .fg_terrain_evidence_check(x, sources)
  if (is.null(x)) return(NULL)
  if (is.null(manifest)) .fg_abort("Retained evidence requires a linked terrain manifest.")
  result <- x
  result$integrity <- vapply(seq_len(nrow(x)), function(i) {
    path <- .fg_manifest_path(dirname(manifest), x$path[i])
    if (!file.exists(path)) return("MISSING")
    if (dir.exists(path)) return("CHANGED")
    tryCatch(if (identical(.fg_file_sha256(path), x$sha256[i])) "MATCH" else "CHANGED",
      error = function(e) "UNREADABLE")
  }, character(1))
  result
}
