#' Add explicitly assigned Reaches to a saved Study Area
#'
#' Creates new local Reach identities under existing Stream identities. Existing
#' Study Area, Streams, Reaches, events and links are preserved. This is additive,
#' not an identity reconciliation, rename, replacement or automatic segmentation.
#' No areas, terrain or acquired Survey Events are required for names-only drafts.
#'
#' @param dsn Existing saved Study Area context GeoPackage.
#' @param output_file New .gpkg beside dsn; never overwrite an existing context.
#' @param reaches Nonempty data frame or polygon sf, one row per new Reach.
#'   Only chosen name, parent reference and optional geometry are imported.
#'   Other attributes, including source Reach IDs, are explicitly ignored.
#' @param name_column Exact text column containing Reach names. Names are trimmed,
#'   nonempty and unique ignoring case within each parent Stream, including
#'   existing Reaches. The same name may occur under different Streams.
#' @param parent_column Exact text column containing existing Stream references.
#' @param parent_key Either stream_id (default) or stream_name. Every reference
#'   must match exactly one saved Stream; no fuzzy or spatial matching occurs.
#' @param add_note Required source and segmentation/assignment rationale, appended
#'   to existing notes. Not an approval signature or structured provenance ledger.
#' @param report_file Optional new .html destination in an existing directory.
#' @param report_purpose Report view: definition (default), terrain or staging.
#' @return List of context and report paths. New Reach IDs are local UUIDs.
#'   Areas, if supplied, must be valid finite nonempty CRS-defined XY polygons.
#'   Appending to an existing Reach table requires the same geometry presence,
#'   polygon type and semantic CRS; no repair/reprojection or missing-AOI invention.
#'   Native geometry is retained. No containment/coverage approval is implied.
#'   Saving and reporting are not one transaction: a rendering failure may leave
#'   the context. Continue that output and retry read-only reporting.
#' @export
add_study_reaches <- function(dsn, output_file, reaches,
    name_column = "reach_name", parent_column = "stream_id",
    parent_key = "stream_id", add_note, report_file = NULL,
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
  parent_key <- .fg_choice(parent_key, c("stream_id", "stream_name"), "parent_key")
  name_column <- .fg_required_text(name_column, "name_column")
  parent_column <- .fg_required_text(parent_column, "parent_column")
  note <- .fg_required_text(add_note, "add_note")
  args <- read_study_context(dsn)
  if (is.null(args$study_area) || is.null(args$streams) || !nrow(args$streams))
    .fg_abort("Supply an existing Study Area and Stream inventory before adding Reaches.")
  if (!is.data.frame(reaches) || !nrow(reaches) || anyDuplicated(names(reaches)) ||
      !all(c(name_column, parent_column) %in% names(reaches)))
    .fg_abort("Supply Reach rows and exact existing name/parent columns.")
  text <- function(x) {
    if (!is.character(x) || is.object(x) || !is.null(dim(x)) || anyNA(x) ||
        any(!nzchar(trimws(x)))) .fg_abort("Reach names and Stream references must be nonempty text.")
    trimws(x)
  }
  labels <- text(reaches[[name_column]])
  refs <- text(reaches[[parent_column]])
  keys <- args$streams[[parent_key]]
  if (any(vapply(refs, function(ref) sum(keys == ref), integer(1)) != 1L))
    .fg_abort("Each parent reference must match exactly one saved Stream; missing or ambiguous parent.")
  parents <- args$streams$stream_id[match(refs, keys)]
  old <- args$reaches
  pairs <- data.frame(stream_id = parents, reach_name = tolower(labels))
  if (!is.null(old)) pairs <- rbind(data.frame(stream_id = old$stream_id,
    reach_name = tolower(trimws(old$reach_name))), pairs)
  if (anyDuplicated(pairs)) .fg_abort("Duplicate Reach name within a Stream; existing Reach identities were not replaced.")
  if (inherits(reaches, "sf")) {
    .fg_terrain_polygon(reaches)
    if (!class(sf::st_geometry(reaches))[1] %in% c("sfc_POLYGON", "sfc_MULTIPOLYGON") ||
        !all(vapply(sf::st_geometry(reaches), inherits, logical(1), "XY")) ||
        !all(is.finite(sf::st_coordinates(reaches)))) .fg_abort("Reach areas require finite XY polygon geometry of one type.")
  }
  result <- data.frame(reach_id = .fg_generate_uuid(nrow(reaches)),
    stream_id = parents, reach_name = labels)
  if (inherits(reaches, "sf")) sf::st_geometry(result) <- sf::st_geometry(reaches)
  if (!is.null(old)) {
    if (inherits(old, "sf") != inherits(result, "sf"))
      .fg_abort("Existing and new Reaches must both have areas or both be names-only; no missing areas were invented.")
    if (inherits(old, "sf")) {
      if (!isTRUE(sf::st_crs(old) == sf::st_crs(result)) ||
          class(sf::st_geometry(old))[1] != class(sf::st_geometry(result))[1])
        .fg_abort("New Reach areas must match the existing Reach CRS and polygon type; no reprojection was attempted.")
      names(result)[names(result) == attr(result, "sf_column")] <- attr(old, "sf_column")
      sf::st_geometry(result) <- attr(old, "sf_column")
    }
    result <- rbind(old, result)
  }
  args$reaches <- result
  note <- paste0("Reaches added (", nrow(reaches), "): ", note)
  args$analyst_notes <- if (is.na(args$analyst_notes)) note else paste(args$analyst_notes, note, sep = "\n\n")
  .fg_save_study_revision(args, dsn, output_file, report_file, purpose)
}
