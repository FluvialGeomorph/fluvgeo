#' Revise a Study Area label or append a scope note in a new context copy
#'
#' A bounded editing operation, not identity reconciliation or an acceptance
#' action. All other supplied records, native AOIs and relative evidence links
#' are retained. The new context must be beside the source; moving an entire
#' folder remains a separate operation. Prior notes are never replaced.
#'
#' @param dsn Existing saved Study Area context GeoPackage.
#' @param output_file New .gpkg in the same directory as dsn.
#' @param study_area_name Optional nonempty replacement display name. NULL keeps
#'   the existing name. A Study Area record must already exist; no ID is created.
#' @param add_note Optional nonempty scope note appended to analyst_notes with a
#'   paragraph break. NULL keeps existing notes. At least one change is required.
#' @param report_file Optional new .html destination in an existing directory.
#'   If supplied, render the existing report from the saved revised context.
#' @return List with context and report paths (report NULL when not requested).
#'   Publication is non-replacing, but the two outputs are not one transaction.
#'   If rendering fails or execution is canceled after saving, the new context
#'   can remain. Retain it for inspection; use read-only reporting to retry.
#'   No full revision ledger or approval signature is created by this operation.
#' @export
revise_study_context <- function(dsn, output_file, study_area_name = NULL,
    add_note = NULL, report_file = NULL) {
  dsn <- .fg_network_dsn(dsn)
  output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file))
    .fg_abort("Save the revised context beside the original to preserve relative links.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  args <- read_study_context(dsn)
  changed <- FALSE
  if (!is.null(study_area_name)) {
    study_area_name <- .fg_required_text(study_area_name, "study_area_name")
    if (is.null(args$study_area)) .fg_abort("No supplied Study Area record to rename; no identity was created.")
    changed <- !identical(study_area_name, args$study_area$study_area_name)
    args$study_area$study_area_name <- study_area_name
  }
  if (!is.null(add_note)) {
    add_note <- .fg_required_text(add_note, "add_note")
    args$analyst_notes <- if (is.na(args$analyst_notes)) add_note else
      paste(args$analyst_notes, add_note, sep = "\n\n")
    changed <- TRUE
  }
  if (!changed) .fg_abort("No changes supplied; choose a new name or add a scope note.")
  # The checked reader resolves links to absolute paths. Restore their saved
  # root-relative representation without changing evidence or copying assets.
  root <- dirname(dsn)
  for (nm in c("network", "folder_manifest")) if (!is.null(args[[nm]])) {
    args[[nm]] <- substring(args[[nm]], nchar(root) + 2L)
    .fg_manifest_path(root, args[[nm]])
  }
  context <- do.call(write_study_context, c(list(dsn = output_file), args))
  report <- NULL
  if (!is.null(report_file)) report <- tryCatch(
    terrain_development_report(read_study_context_summary(context), report_file),
    error = function(e) .fg_abort(paste("Revised context saved at", context,
      "but report generation failed:", conditionMessage(e),
      "Keep the context for inspection and retry with read-only reporting.")))
  list(context = context, report = report)
}
