#' Define the initial Streams of a saved Study Area
#'
#' Creates new local Stream identities for an explicitly chosen initial inventory.
#' This is not an append, replacement, or archived/enterprise ID reconciliation
#' operation. Existing Streams or linked network/hierarchy records are refused.
#' No Study Area boundary, terrain, Reach or acquired Survey Event is required.
#'
#' @param dsn Existing saved Study Area context GeoPackage.
#' @param output_file New .gpkg beside dsn; existing files are refused.
#' @param streams Nonempty data frame or sf with one row per chosen Stream.
#'   Optional areas must be valid nonempty finite XY POLYGON/MULTIPOLYGON geometry
#'   with a known CRS. Native coordinates/CRS are retained. No dissolve, clipping,
#'   reprojection or containment/coverage acceptance is performed.
#' @param name_column Exact character column containing explicit Stream names.
#'   Only this column and optional geometry are imported. Other source attributes,
#'   including any identities, are not adopted. Names are trimmed and must be
#'   nonempty and unique ignoring case for this initial-definition interface.
#' @param add_note Required source and naming/segmentation rationale. Appended to
#'   existing notes; not an approval signature or structured provenance ledger.
#' @param report_file Optional new .html path in an existing directory.
#' @param report_purpose Report view, default definition; terrain/staging supported.
#' @return List of context and report paths, with report NULL when not requested.
#'   Existing context and other records/links are retained. Saving and rendering
#'   are not one transaction: render failure can leave the saved context. Reopen
#'   it for report recovery rather than generating another set of identities.
#' @export
define_study_streams <- function(dsn, output_file, streams,
    name_column = "stream_name", add_note, report_file = NULL,
    report_purpose = "definition") {
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
  note <- .fg_required_text(add_note, "add_note")
  name_column <- .fg_required_text(name_column, "name_column")
  args <- read_study_context(dsn)
  if (is.null(args$study_area)) .fg_abort("Supply an existing Study Area identity before defining Streams.")
  if (any(vapply(args[c("streams", "reaches", "survey_events", "network")],
      function(x) !is.null(x), logical(1))))
    .fg_abort("Streams or related hierarchy/network already exist; use identity-aware editing, not initial Stream definition.")
  if (!is.data.frame(streams) || nrow(streams) < 1L || anyDuplicated(names(streams)) ||
      !name_column %in% names(streams))
    .fg_abort("Supply Stream rows and an exact existing name_column.")
  names <- streams[[name_column]]
  if (!is.character(names) || is.object(names) || !is.null(dim(names)) || anyNA(names) ||
      any(!nzchar(trimws(names)))) .fg_abort("Stream names must be nonempty character values.")
  names <- trimws(names)
  if (anyDuplicated(tolower(names))) .fg_abort("Duplicate Stream names are ambiguous in this initial-definition tool.")
  if (inherits(streams, "sf")) {
    .fg_terrain_polygon(streams)
    if (!all(vapply(sf::st_geometry(streams), inherits, logical(1), "XY")) ||
        !all(is.finite(sf::st_coordinates(streams)))) .fg_abort("Stream areas require finite XY polygon geometry.")
  }
  result <- data.frame(stream_id = .fg_generate_uuid(nrow(streams)),
    study_area_id = args$study_area$study_area_id, stream_name = names)
  if (inherits(streams, "sf")) sf::st_geometry(result) <- sf::st_geometry(streams)
  args$streams <- result
  note <- paste0("Initial Streams defined (", nrow(result), "): ", note)
  args$analyst_notes <- if (is.na(args$analyst_notes)) note else paste(args$analyst_notes, note, sep = "\n\n")
  .fg_save_study_revision(args, dsn, output_file, report_file, purpose)
}
