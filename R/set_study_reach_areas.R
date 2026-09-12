#' Supply or revise areas of existing Study Reaches
#'
#' Matches explicit saved Reach IDs, never names, row positions or spatial overlap.
#' Only geometry changes; all identities, names, parentage, events and links remain.
#' This does not define how a Reach area should be delineated or approve its scope.
#'
#' @param dsn Existing context GeoPackage containing Reaches.
#' @param output_file New .gpkg beside dsn; existing files are refused.
#' @param areas Nonempty sf of valid finite CRS-defined XY polygons, one per
#'   explicitly selected existing Reach. Native geometry is retained.
#' @param id_column Exact text field containing saved reach_id values, default
#'   reach_id. Every value must match a different existing Reach. Other source
#'   attributes are ignored, not used to rename or reparent records.
#' @param add_note Required source and delineation/revision rationale.
#' @param report_file Optional new .html destination in an existing directory.
#' @param report_purpose Report view: definition (default), terrain or staging.
#' @return List of context and report paths. When Reaches have no geometry, supply
#'   every recorded Reach's area together: schema 1 cannot mix missing and supplied
#'   areas. Once areas exist, a subset may be revised in the same semantic CRS and
#'   polygon class. No repair, dissolve, reprojection, clipping, new identities,
#'   Survey Events or source-ID reconciliation is performed. Existing outputs are
#'   never replaced. Rendering failure may leave the saved context; keep it and
#'   retry read-only reporting. A rationale is not a full provenance ledger.
#' @export
set_study_reach_areas <- function(dsn, output_file, areas, id_column = "reach_id",
    add_note, report_file = NULL, report_purpose = "definition") {
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
  id_column <- .fg_required_text(id_column, "id_column")
  note <- .fg_required_text(add_note, "add_note")
  args <- read_study_context(dsn); old <- args$reaches
  if (is.null(old) || !nrow(old)) .fg_abort("Define Reaches before assigning their areas; no identities were created.")
  if (!inherits(areas, "sf") || !nrow(areas) || anyDuplicated(names(areas)) || !id_column %in% names(areas))
    .fg_abort("Supply polygon areas with the exact existing ID column.")
  ids <- areas[[id_column]]
  if (!is.character(ids) || is.object(ids) || !is.null(dim(ids)) || anyNA(ids) ||
      anyDuplicated(ids) || any(!ids %in% old$reach_id))
    .fg_abort("Each area must identify a different existing Reach by its exact saved ID.")
  .fg_terrain_polygon(areas)
  g <- sf::st_geometry(areas)
  if (!class(g)[1] %in% c("sfc_POLYGON", "sfc_MULTIPOLYGON") ||
      !all(vapply(g, inherits, logical(1), "XY")) || !all(is.finite(sf::st_coordinates(areas))))
    .fg_abort("Reach areas require finite XY polygon geometry of one type.")
  if (!inherits(old, "sf")) {
    if (length(ids) != nrow(old))
      .fg_abort("Supply areas for every recorded Reach together; mixed missing/supplied areas are not supported by this context format.")
    sf::st_geometry(old) <- g[match(old$reach_id, ids)]
    action <- "supplied"
  } else {
    if (!isTRUE(sf::st_crs(old) == sf::st_crs(areas)) || class(sf::st_geometry(old))[1] != class(g)[1])
      .fg_abort("Replacement areas must match the existing Reach CRS and polygon type; no reprojection was attempted.")
    geometry <- sf::st_geometry(old)
    geometry[match(ids, old$reach_id)] <- g
    sf::st_geometry(old) <- geometry
    action <- "revised"
  }
  args$reaches <- old
  note <- paste0("Reach areas ", action, " (", length(ids), "): ", note)
  args$analyst_notes <- if (is.na(args$analyst_notes)) note else paste(args$analyst_notes, note, sep = "\n\n")
  .fg_save_study_revision(args, dsn, output_file, report_file, purpose)
}
