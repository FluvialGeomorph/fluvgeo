#' Record one acquired Survey Event under an existing Reach
#'
#' Creates a local event identity without changing hierarchy, areas, existing
#' events, notes or evidence links. This is analyst-supplied inventory, not
#' independent acquisition verification, asset linking or FGDB acceptance.
#'
#' @param dsn Existing context GeoPackage with the parent Reach.
#' @param output_file New .gpkg beside dsn; never overwrite existing context.
#' @param reach_id Exact existing Reach UUID; no parentage is inferred.
#' @param acquired_date Known date as YYYY, YYYY-MM or YYYY-MM-DD. Unknown parts
#'   remain missing integers. Invalid dates and periods entirely in the future
#'   are refused; planned/undated work belongs in scope/reconstruction notes.
#' @param source_dataset Nonempty source/delivery reference stored as text.
#'   Not opened, copied, verified or pinned as an asset link.
#' @param evidence_note Nonempty basis for acquisition/date interpretation,
#'   stored in availability_notes, not duplicated in Study Area notes.
#' @param report_file Optional new .html destination in an existing directory.
#' @param report_purpose Report view: definition (default), terrain or staging.
#' @return List of context and report paths. Exact repeats of Reach, date
#'   precision/value and trimmed source reference are refused. Alternate labels
#'   or precision still need analyst duplicate review; this is not reconciliation.
#'   Existing optional columns retain values; absent columns gain typed missing
#'   values. Rendering failure may leave the context; keep it and retry read-only
#'   reporting. No acquisition or terrain availability is inferred.
#' @export
record_study_survey_event <- function(dsn, output_file, reach_id, acquired_date,
    source_dataset, evidence_note, report_file = NULL, report_purpose = "definition") {
  purpose <- .fg_choice(report_purpose, c("terrain", "definition", "staging"), "report_purpose")
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file)) .fg_abort("Save the revised context beside the original to preserve relative links.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  reach_id <- .fg_required_text(reach_id, "reach_id")
  date <- .fg_required_text(acquired_date, "acquired_date")
  source <- trimws(.fg_required_text(source_dataset, "source_dataset"))
  note <- .fg_required_text(evidence_note, "evidence_note")
  if (!grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", date))
    .fg_abort("Supply acquired_date as YYYY, YYYY-MM or YYYY-MM-DD; do not invent missing date parts.")
  parts <- as.integer(strsplit(date, "-", fixed = TRUE)[[1]])
  event <- data.frame(survey_event_id = .fg_generate_uuid(1), reach_id = reach_id,
    survey_year = parts[1], survey_month = parts[2], survey_day = parts[3],
    source_dataset = source, availability_notes = note)
  label <- tryCatch(.fg_terrain_dates(event), error = function(e)
    .fg_abort("acquired_date must contain a valid calendar year, month and day at the supplied precision."))
  earliest <- as.Date(paste0(label, switch(as.character(length(parts)),
    `1` = "-01-01", `2` = "-01", `3` = "")))
  if (earliest > Sys.Date()) .fg_abort("Acquired Survey Events cannot be entirely in the future; keep plans in scope notes.")
  args <- read_study_context(dsn)
  if (is.null(args$reaches) || !reach_id %in% args$reaches$reach_id)
    .fg_abort("Select an exact existing Reach ID; no parent or Reach was created.")
  old <- args$survey_events
  if (!is.null(old)) {
    for (nm in setdiff(names(event), names(old)))
      old[[nm]] <- if (nm %in% c("survey_month", "survey_day")) rep(NA_integer_, nrow(old)) else rep(NA_character_, nrow(old))
    same_source <- !is.na(old$source_dataset) & trimws(old$source_dataset) == source
    if (any(old$reach_id == reach_id & .fg_terrain_dates(old) == label & same_source))
      .fg_abort("This Reach, acquisition date and source are already recorded; existing event identities were not replaced.")
    event <- rbind(old[names(event)], event)
  }
  args$survey_events <- event
  .fg_save_study_revision(args, dsn, output_file, report_file, purpose)
}
