#' Start a new Study Area draft without acquired data
#'
#' Creates a named Study Area and optional purpose/scope notes in the existing
#' saved-context binding. This is for a genuinely new study, not recovering an
#' existing identity from an archive. No boundary, CRS, Stream, Reach, Survey
#' Event, terrain reference or acceptance is inferred. A new local UUID is
#' generated once and retained by subsequent context revisions.
#'
#' @param output_file New .gpkg path in an existing directory.
#' @param study_area_name Nonempty working Study Area name.
#' @param analyst_notes Optional nonempty purpose/scope text, or NA_character_.
#'   Stored as the existing analyst_notes field, not structured requirements,
#'   an acquisition record or an approval signature. Outer whitespace is trimmed.
#' @param report_file Optional new .html path in an existing directory. Renders
#'   define_study_area_report() from the reopened saved context.
#' @return List of context and report paths (report NULL if not requested).
#'   Existing destinations are refused. Outputs are not one transaction: a
#'   rendering failure or cancellation after saving can leave the context.
#'   Keep it and retry reporting without creating another study identity.
#' @export
start_study_context <- function(output_file, study_area_name,
    analyst_notes = NA_character_, report_file = NULL) {
  output_file <- .fg_network_dsn(output_file)
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  study_area_name <- .fg_required_text(study_area_name, "study_area_name")
  analyst_notes <- .fg_optional_text(analyst_notes, "analyst_notes")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  context <- write_study_context(output_file, study_area = data.frame(
    study_area_id = .fg_generate_uuid(1L), study_area_name = study_area_name),
    analyst_notes = analyst_notes)
  report <- NULL
  if (!is.null(report_file)) report <- tryCatch(
    define_study_area_report(read_study_context_summary(context), report_file),
    error = function(e) .fg_abort(paste("Draft context saved at", context,
      "but report generation failed:", conditionMessage(e),
      "Keep the context; retry define_study_area_report(read_study_context_summary(context),",
      "a new report path) rather than starting another study.")))
  list(context = context, report = report)
}

#' Render a prospective Define Study Area report
#'
#' A small design view of shared context: supplied scope and next design
#' conversations, without legacy-staging prerequisites or terrain-quality gates.
#' Omitted boundaries and hierarchy are open choices, not historical defects.
#' This is not the complete planning editor or a readiness certificate. Report
#' purpose is not persisted and rendering does not change identities or evidence.
#'
#' @param summary Output of terrain_development_summary(), including a draft
#'   reopened by read_study_context_summary(). Actual Survey Events remain dated
#'   evidence; intended observations can be discussed in notes, not event rows.
#' @param output_file New .html path in an existing directory; never overwritten.
#' @return Normalized path invisibly. Requires knitr, Pandoc and a local
#'   hard-link-capable filesystem for non-replacing publication.
#' @export
define_study_area_report <- function(summary, output_file) {
  .fg_render_study_report(summary, output_file, "define_study_area_report.Rmd")
}
