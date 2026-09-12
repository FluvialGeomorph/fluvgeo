#' Assemble read-only terrain reference evidence for review
#'
#' Keeps file declarations, caller-recorded analysis choices and preparation
#' evidence separate. Differences between source and analysis CRS are not
#' automatically conflicts. No source lineage, metadata acceptance or conversion
#' is inferred. Selected files are inspected now; rendering uses that snapshot.
#'
#' @param artifacts Data frame with unique artifact_id, label, role, path and
#'   evidence character columns. role is SOURCE_PRODUCT or ANALYSIS_DEM; evidence
#'   describes the supplied role/selection, not scientific acceptance. NA path
#'   means no local GeoTIFF selected. Optional review_note/review_evidence and
#'   preparation_note/preparation_evidence pairs retain attributed supplied text.
#'   A note requires its evidence; preparation notes apply to ANALYSIS_DEM only.
#' @param analysis_reference Optional data frame with component, value, basis,
#'   evidence character columns. Unique components are horizontal, vertical and
#'   elevation_unit. Basis is PROJECT_RECORD, OWNER_RECOLLECTION or PROPOSED.
#'   Omitted components remain unresolved; supplied values require evidence.
#' @return A list of class fg_terrain_reference_review with files, analysis_reference,
#'   observations keyed by artifact_id, and generated_at. Per-file inspection
#'   failures remain FILE_INSPECTION_FAILED findings, not absent-CRS claims.
#' @seealso [inspect_terrain_vertical_reference()], [survey_opportunity_summary()]
#' @md
#' @export
terrain_reference_review <- function(artifacts, analysis_reference = NULL) {
  fields <- c("artifact_id", "label", "role", "path", "evidence")
  .fg_require_table(artifacts, fields, "artifacts")
  for (field in setdiff(fields, "path"))
    .fg_required_text(artifacts[[field]], field, nrow(artifacts))
  if (anyDuplicated(artifacts$artifact_id) ||
      any(!artifacts$role %in% c("SOURCE_PRODUCT", "ANALYSIS_DEM")))
    .fg_abort("Artifact IDs must be unique and roles SOURCE_PRODUCT or ANALYSIS_DEM.")
  if (!is.character(artifacts$path) || any(!is.na(artifacts$path) & !nzchar(trimws(artifacts$path))))
    .fg_abort("path must be character; use NA when no local GeoTIFF is selected.")
  files <- artifacts[fields]
  for (field in c("review_note", "review_evidence", "preparation_note", "preparation_evidence")) {
    v <- artifacts[[field]]
    if (is.null(v)) v <- rep(NA_character_, nrow(files))
    if (!is.character(v)) .fg_abort("Notes and their evidence must be character.")
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    files[[field]] <- v
  }
  for (pair in c("review", "preparation")) {
    note <- files[[paste0(pair, "_note")]]
    evidence <- files[[paste0(pair, "_evidence")]]
    if (any(xor(is.na(note), is.na(evidence))))
      .fg_abort("Supply each note together with its attributed evidence.")
  }
  if (any(!is.na(files$preparation_note) & files$role != "ANALYSIS_DEM"))
    .fg_abort("Preparation notes belong to ANALYSIS_DEM artifacts.")
  analysis <- data.frame(component = c("horizontal", "vertical", "elevation_unit"),
    value = NA_character_, basis = "UNRESOLVED", evidence = NA_character_)
  if (!is.null(analysis_reference)) {
    .fg_require_table(analysis_reference, names(analysis), "analysis_reference")
    for (field in names(analysis))
      .fg_required_text(analysis_reference[[field]], field, nrow(analysis_reference))
    if (anyDuplicated(analysis_reference$component) ||
        any(!analysis_reference$component %in% analysis$component) ||
        any(!analysis_reference$basis %in% c("PROJECT_RECORD", "OWNER_RECOLLECTION", "PROPOSED")))
      .fg_abort("Supply unique supported analysis components and evidence bases.")
    analysis[match(analysis_reference$component, analysis$component), ] <- analysis_reference[names(analysis)]
  }
  observations <- setNames(vector("list", nrow(files)), files$artifact_id)
  files$inspection_status <- rep("NOT_SELECTED", nrow(files))
  files$horizontal <- files$vertical <- files$vertical_unit <- files$band_unit <- rep("Not inspected", nrow(files))
  files$reader_note <- rep("No local GeoTIFF selected", nrow(files))
  files$next_action <- rep("Select a local GeoTIFF or retain external evidence; no CRS conclusion yet.", nrow(files))
  for (i in seq_len(nrow(files))) {
    if (is.na(files$path[i])) next
    observation <- tryCatch(inspect_terrain_vertical_reference(files$path[i]),
      error = function(e) list(error = conditionMessage(e)))
    observations[i] <- list(observation)
    if (!is.null(observation$error)) {
      files$inspection_status[i] <- "FILE_INSPECTION_FAILED"
      files$reader_note[i] <- "Selected file could not be inspected"
      files$next_action[i] <- "Resolve the file/reader issue; do not interpret failure as missing vertical metadata."
      next
    }
    view <- observation$internal_compound
    files$inspection_status[i] <- view$status
    files$horizontal[i] <- .fg_reference_horizontal(view$projjson)
    files$vertical[i] <- if (!is.null(view$vertical_crs$name)) view$vertical_crs$name else
      if (view$status == "VERTICAL_CRS_EXPOSED") "Exposed; see saved definition" else "Not exposed by this inspection"
    files$vertical_unit[i] <- .fg_reference_unit(view$vertical_crs$coordinate_system$axis[[1L]]$unit)
    files$band_unit[i] <- if (nzchar(view$band_unit)) view$band_unit else "Not exposed"
    files$reader_note[i] <- if (observation$crs_text_differs)
      "Reader views differ; inspect definitions before interpreting the difference" else "Reader CRS text agrees"
    files$next_action[i] <- if (view$status == "VERTICAL_CRS_EXPOSED")
      "Check the declaration against preparation/source evidence; it does not establish scientific acceptance." else
      "Recover vertical reference and elevation units from preparation/source evidence; do not infer them from horizontal CRS."
  }
  structure(list(schema = "TERRAIN_REFERENCE_REVIEW_1", files = files,
    analysis_reference = analysis, observations = observations,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    class = "fg_terrain_reference_review")
}

.fg_reference_horizontal <- function(crs) {
  if (is.null(crs)) return("Not exposed in structured metadata; see saved definition")
  if (identical(crs$type, "BoundCRS")) return(.fg_reference_horizontal(crs$source_crs))
  if (identical(crs$type, "CompoundCRS")) {
    candidates <- Filter(function(x) !identical(x$type, "VerticalCRS"), crs$components)
    if (length(candidates)) return(.fg_reference_horizontal(candidates[[1L]]))
  }
  if (crs$type %in% c("ProjectedCRS", "GeographicCRS", "GeodeticCRS") && !is.null(crs$name))
    return(crs$name)
  "Not exposed in structured metadata; see saved definition"
}

.fg_reference_unit <- function(unit) {
  if (is.null(unit)) return("Not exposed as a vertical CRS unit")
  if (is.character(unit)) return(unit)
  if (is.null(unit$name)) return("See saved unit definition")
  if (!is.null(unit$conversion_factor) && identical(unit$type, "LinearUnit"))
    return(paste0(unit$name, " (", format(unit$conversion_factor, digits = 16, trim = TRUE), " m/unit)"))
  unit$name
}
