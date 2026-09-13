#' Record an attributed account of terrain preparation
#'
#' Saves explicitly supplied, ordered preparation steps linked to one terrain
#' source-use account. This records an account, not an executable recipe or proof
#' of execution. Nothing is fetched, parsed, transformed or inferred.
#'
#' @param dsn Existing saved Study Area context containing source-use records.
#' @param output_file New context .gpkg beside dsn; earlier snapshots are retained.
#' @param processing_id Unique local account ID, append-only. Corrected accounts
#'   use a new ID and explain the correction in qualifications.
#' @param association_id Exact source-use account ID. Its target fingerprint
#'   identifies the inventoried derivative; it does not fingerprint step inputs.
#' @param steps Nonempty nonspatial data frame with exactly seven plain character
#'   columns: operation, input_description, output_description, parameters,
#'   software, software_version, and execution_time. Each operation is required;
#'   other cells may be NA for unknown. Row order is the analyst-declared order,
#'   not independently established execution order. Descriptions may name multiple
#'   inputs/outputs but do not resolve product identities or graph relationships.
#'   Parameters are literal text, never evaluated. execution_time retains known
#'   date precision as text; it is not the time this account is recorded.
#' @param basis PROJECT_RECORD or OWNER_RECOLLECTION, never verified execution.
#' @param qualifications Nonempty support, gaps, limitations and any correction
#'   rationale. Do not supply invented details to fill missing archive evidence.
#' @param analyst Nonempty recorder attribution, not an approval signature.
#' @param evidence_id Optional retained PROCESSING_RECORD ID belonging to the
#'   same source-use account. NA means no retained document is linked. Its current
#'   integrity is reported separately and does not authenticate this account.
#' @param report_file Optional new HTML report path.
#' @param report_purpose definition (default), terrain, or staging.
#' @return List with context and report paths. Uses FLUVGEO_STUDY_CONTEXT_5;
#'   earlier schemas remain readable. Existing editors preserve the account.
#'   A report failure retains the saved context for reporting retry. This does not
#'   promote source use, change scientific assessment or establish comparability.
#' @export
record_study_terrain_processing <- function(dsn, output_file, processing_id,
    association_id, steps, basis, qualifications, analyst,
    evidence_id = NA_character_, report_file = NULL, report_purpose = "definition") {
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
  fields <- .fg_terrain_processing_step_fields()
  if (!is.data.frame(steps) || inherits(steps, "sf") || !nrow(steps) ||
      anyDuplicated(names(steps)) || !setequal(names(steps), fields))
    .fg_abort("steps requires nonempty records with exactly the seven supported fields.")
  before <- .fg_file_sha256(dsn)
  args <- read_study_context(dsn)
  manifest_hash <- if (!is.null(args$folder_manifest)) .fg_file_sha256(args$folder_manifest) else NULL
  row <- data.frame(processing_id = rep(.fg_required_text(processing_id, "processing_id"), nrow(steps)),
    association_id = .fg_required_text(association_id, "association_id"),
    step_number = seq_len(nrow(steps)), steps[fields],
    basis = .fg_required_text(basis, "basis"),
    evidence_id = .fg_optional_text(evidence_id, "evidence_id"),
    qualifications = .fg_required_text(qualifications, "qualifications"),
    analyst = .fg_required_text(analyst, "analyst"),
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  old <- args$terrain_processing
  if (!is.null(old) && processing_id %in% old$processing_id)
    .fg_abort("Processing account IDs are append-only; use a new ID for a qualified correction.")
  args$terrain_processing <- if (is.null(old)) row else rbind(old, row)
  .fg_terrain_processing_check(args$terrain_processing, args$terrain_sources, args$terrain_evidence)
  if (!identical(before, .fg_file_sha256(dsn)) ||
      !identical(manifest_hash, .fg_file_sha256(args$folder_manifest)))
    .fg_abort("Context or manifest changed during recording; retry against stable evidence.")
  .fg_save_study_revision(args, dsn, output_file, report_file, report_purpose)
}

.fg_terrain_processing_step_fields <- function() c("operation", "input_description",
  "output_description", "parameters", "software", "software_version", "execution_time")

.fg_terrain_processing_fields <- function() c("processing_id", "association_id", "step_number",
  .fg_terrain_processing_step_fields(), "basis", "evidence_id", "qualifications", "analyst", "recorded_at")

.fg_terrain_processing_check <- function(x, sources, evidence) {
  if (is.null(x)) return(invisible(NULL))
  fields <- .fg_terrain_processing_fields()
  if (!is.data.frame(x) || inherits(x, "sf") || !nrow(x) ||
      anyDuplicated(names(x)) || !setequal(names(x), fields))
    .fg_abort("terrain_processing requires nonempty records with exactly the supported fields.")
  optional <- c(setdiff(.fg_terrain_processing_step_fields(), "operation"), "evidence_id")
  for (field in setdiff(fields, "step_number")) {
    v <- x[[field]]
    if (!is.character(v) || is.object(v) || !is.null(dim(v)))
      .fg_abort("Processing text fields must be plain character columns.")
    if (field %in% optional) v <- v[!is.na(v)]
    if (length(v)) .fg_required_text(v, field, length(v))
  }
  if (!is.integer(x$step_number) || is.object(x$step_number) ||
      !is.null(dim(x$step_number)) || anyNA(x$step_number))
    .fg_abort("step_number must be a plain integer column without missing values.")
  if (is.null(sources) || any(!x$association_id %in% sources$association_id))
    .fg_abort("Processing accounts require an existing source-use association.")
  if (any(!x$basis %in% c("PROJECT_RECORD", "OWNER_RECOLLECTION")))
    .fg_abort("Unsupported processing evidence basis; execution is not verified.")
  header <- c("association_id", "basis", "evidence_id", "qualifications", "analyst", "recorded_at")
  for (id in unique(x$processing_id)) {
    rows <- x[x$processing_id == id, , drop = FALSE]
    if (!identical(sort(rows$step_number), seq_len(nrow(rows))))
      .fg_abort("Each processing account needs unique contiguous step numbers starting at 1.")
    if (any(vapply(rows[header], function(v) length(unique(v)) != 1L, logical(1))))
      .fg_abort("Processing account attribution and links must agree across its steps.")
  }
  linked <- !is.na(x$evidence_id)
  if (any(linked)) {
    i <- match(x$evidence_id[linked], evidence$evidence_id)
    if (anyNA(i) || any(evidence$kind[i] != "PROCESSING_RECORD") ||
        any(evidence$association_id[i] != x$association_id[linked]))
      .fg_abort("Select a retained PROCESSING_RECORD from the same source-use association.")
  }
  parsed <- as.POSIXct(strptime(x$recorded_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (anyNA(parsed) || any(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != x$recorded_at))
    .fg_abort("recorded_at must be a valid UTC timestamp in YYYY-MM-DDTHH:MM:SSZ form.")
  invisible(NULL)
}
