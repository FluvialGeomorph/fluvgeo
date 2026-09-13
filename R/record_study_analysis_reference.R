#' Record a Study Area analysis-reference choice in a new context
#'
#' Records or explicitly revises one project-wide analysis choice, not a source
#' declaration, executed transformation, or approval. Values are descriptive text,
#' not validated CRS definitions or unit-conversion instructions. Prior context
#' files, other components, terrain and manifest assertions are preserved.
#'
#' @param dsn Existing saved Study Area context GeoPackage.
#' @param output_file New context .gpkg beside dsn.
#' @param component One of horizontal, vertical, elevation_unit.
#' @param value Nonempty description of the choice. State exceptions/limitations
#'   in evidence; a project-wide record does not certify every event or DEM.
#' @param basis PROJECT_RECORD, OWNER_RECOLLECTION, or PROPOSED. A proposal is not
#'   an established choice; corroboration is not inferred from saving.
#' @param evidence Nonempty source, rationale and qualifications for this choice.
#' @param analyst Nonempty recorder attribution, not an approval signature.
#' @param report_file Optional new HTML report. Saved choices are visible without
#'   opting in to fresh terrain-file inspection.
#' @param report_purpose definition (default), terrain, or staging.
#' @return List with context and report paths. Schema-1 inputs remain readable;
#'   contexts containing choices use FLUVGEO_STUDY_CONTEXT_2. New-file publication
#'   preserves the source. A later report failure retains the new context for
#'   inspection and retry. No in-file revision ledger or cryptographic signature
#'   is created; retain prior snapshots. No missing choice is inferred.
#' @export
record_study_analysis_reference <- function(dsn, output_file, component, value,
    basis, evidence, analyst, report_file = NULL, report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file)) .fg_abort("Save the revised context beside the original.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  report_purpose <- .fg_choice(report_purpose, c("terrain", "definition", "staging"), "report_purpose")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  row <- data.frame(component = .fg_required_text(component, "component"),
    value = .fg_required_text(value, "value"), basis = .fg_required_text(basis, "basis"),
    evidence = .fg_required_text(evidence, "evidence"), analyst = .fg_required_text(analyst, "analyst"),
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  .fg_study_analysis_check(row)
  before <- .fg_file_sha256(dsn)
  args <- read_study_context(dsn)
  if (is.null(args$study_area)) .fg_abort("A supplied Study Area is required; no identity was created.")
  old <- args$analysis_reference
  if (is.null(old)) old <- row[FALSE, ]
  i <- match(row$component, old$component)
  if (!is.na(i)) {
    fields <- setdiff(names(row), "recorded_at")
    if (all(vapply(fields, function(k) identical(old[[k]][i], row[[k]]), logical(1))))
      .fg_abort("No change supplied; the recorded choice and attribution are unchanged.")
    old[i, ] <- row
  } else old <- rbind(old, row)
  args$analysis_reference <- old
  if (!identical(before, .fg_file_sha256(dsn)))
    .fg_abort("Context changed during recording; retry against a stable snapshot.")
  .fg_save_study_revision(args, dsn, output_file, report_file, report_purpose)
}

.fg_study_analysis_check <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  fields <- c("component", "value", "basis", "evidence", "analyst", "recorded_at")
  if (!is.data.frame(x) || inherits(x, "sf") || !setequal(names(x), fields) ||
      anyDuplicated(names(x)) || !nrow(x))
    .fg_abort("Saved analysis_reference requires nonempty attributed component records with exactly the supported fields.")
  for (field in fields) {
    v <- x[[field]]
    if (!is.character(v) || is.object(v) || !is.null(dim(v)))
      .fg_abort("Saved analysis_reference fields must be plain character columns.")
    .fg_required_text(v, field, nrow(x))
  }
  .fg_reference_analysis(x)
  stamp <- x$recorded_at
  parsed <- as.POSIXct(strptime(stamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (anyNA(parsed) || any(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != stamp))
    .fg_abort("recorded_at must be a valid UTC timestamp in YYYY-MM-DDTHH:MM:SSZ form.")
  invisible(NULL)
}
