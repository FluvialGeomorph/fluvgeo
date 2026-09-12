#' Revise Study Area details or boundary in a new context copy
#'
#' A bounded editing operation, not identity reconciliation or an acceptance
#' action. Other supplied records, child AOIs and relative evidence links
#' are retained. The Study Area AOI changes only when explicitly supplied.
#' The new context must be beside the source; moving an entire
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
#' @param report_purpose Report view: terrain (default, preserving existing
#'   behavior), definition, or staging. Validated before saving. View selection
#'   alone is not an edit; use study_context_report() to change only the report.
#' @param study_area_boundary Optional sf object containing exactly one valid,
#'   nonempty XY POLYGON or MULTIPOLYGON with a known CRS. Copies geometry only,
#'   retaining the existing Study Area identity/name; input attributes are not
#'   imported. Supply a nonempty add_note explaining its source and rationale.
#'   Multiple features must first be deliberately combined by the analyst.
#'   No dissolve, repair, reprojection, clipping or child-boundary changes occur.
#' @return List with context and report paths (report NULL when not requested).
#'   Publication is non-replacing, but the two outputs are not one transaction.
#'   If rendering fails or execution is canceled after saving, the new context
#'   can remain. Retain it for inspection; use read-only reporting to retry.
#'   No full revision ledger or approval signature is created by this operation.
#' @export
revise_study_context <- function(dsn, output_file, study_area_name = NULL,
    add_note = NULL, report_file = NULL, report_purpose = "terrain",
    study_area_boundary = NULL) {
  report_purpose <- .fg_choice(report_purpose, c("terrain", "definition", "staging"), "report_purpose")
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
  if (!is.null(study_area_boundary)) {
    if (is.null(args$study_area))
      .fg_abort("No supplied Study Area record for a boundary; no identity was created.")
    if (is.null(add_note)) .fg_abort("Supply add_note with the boundary source and rationale.")
    add_note <- .fg_required_text(add_note, "add_note")
    x <- study_area_boundary
    if (!inherits(x, "sf") || nrow(x) != 1L || is.na(sf::st_crs(x)) ||
        !as.character(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON") ||
        !inherits(sf::st_geometry(x)[[1]], "XY"))
      .fg_abort("Supply exactly one CRS-defined XY POLYGON or MULTIPOLYGON boundary feature.")
    if (sf::st_is_empty(x) || !all(is.finite(sf::st_coordinates(x))) ||
        !isTRUE(sf::st_is_valid(x)))
      .fg_abort("Boundary must be nonempty, finite and valid; no automatic repair was attempted.")
    action <- if (inherits(args$study_area, "sf")) "replaced" else "supplied"
    sf::st_geometry(args$study_area) <- sf::st_geometry(x)
    add_note <- paste0("Study Area boundary ", action, ": ", add_note)
    changed <- TRUE
  }
  if (!is.null(study_area_name)) {
    study_area_name <- .fg_required_text(study_area_name, "study_area_name")
    if (is.null(args$study_area)) .fg_abort("No supplied Study Area record to rename; no identity was created.")
    changed <- changed || !identical(study_area_name, args$study_area$study_area_name)
    args$study_area$study_area_name <- study_area_name
  }
  if (!is.null(add_note)) {
    add_note <- .fg_required_text(add_note, "add_note")
    args$analyst_notes <- if (is.na(args$analyst_notes)) add_note else
      paste(args$analyst_notes, add_note, sep = "\n\n")
    changed <- TRUE
  }
  if (!changed) .fg_abort("No changes supplied; choose a new name or add a scope note.")
  .fg_save_study_revision(args, dsn, output_file, report_file, report_purpose)
}

.fg_save_study_revision <- function(args, dsn, output_file, report_file, report_purpose) {
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
    study_context_report(context, report_file, report_purpose),
    error = function(e) .fg_abort(paste("Revised context saved at", context,
      "but report generation failed:", conditionMessage(e),
      "Keep the context for inspection and retry with read-only reporting.")))
  list(context = context, report = report)
}
