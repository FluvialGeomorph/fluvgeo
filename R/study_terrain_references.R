# Bind observations to explicit saved selections; do not discover or relabel inputs.
.fg_study_terrain_references <- function(summary, manifest, analysis_reference) {
  links <- summary$event_artifacts
  if (is.null(links)) links <- data.frame(artifact_id = character(),
    survey_event_id = character(), evidence = character(), analyst = character(),
    use_for_report = logical(), grid_status = character())
  links <- links[links$use_for_report, , drop = FALSE]
  ids <- unique(links$artifact_id)
  artifacts <- data.frame(artifact_id = ids, label = ids,
    role = rep("ANALYSIS_DEM", length(ids)), path = rep(NA_character_, length(ids)),
    evidence = rep(NA_character_, length(ids)))
  inventory <- summary$folder_inventory$artifacts
  recorded <- if (is.null(manifest)) list() else jsonlite::read_json(manifest)$artifacts
  hashes <- setNames(vapply(recorded, function(a) a$sha256, character(1)),
    vapply(recorded, function(a) a$artifact_id, character(1)))
  blocked <- character()
  for (i in seq_along(ids)) {
    rows <- links[links$artifact_id == ids[i], , drop = FALSE]
    event <- match(rows$survey_event_id, summary$surveys$survey_event_id)
    labels <- rows$survey_event_id
    known <- !is.na(event)
    labels[known] <- paste(summary$surveys$reach_name[event[known]],
      summary$surveys$date_label[event[known]], sep = " / ")
    artifacts$label[i] <- paste(labels, collapse = "; ")
    artifacts$evidence[i] <- paste(paste0("Event ", rows$survey_event_id, ": ",
      rows$evidence, " (analyst: ", rows$analyst, ")"), collapse = "; ")
    a <- match(ids[i], inventory$artifact_id)
    if (all(rows$grid_status == "GRID_LOADED")) {
      artifacts$path[i] <- .fg_manifest_path(dirname(manifest), inventory$path[a])
    } else blocked <- c(blocked, ids[i])
  }
  review <- terrain_reference_review(artifacts, analysis_reference)
  for (id in ids) {
    i <- match(id, review$files$artifact_id)
    o <- review$observations[[id]]
    if (!is.null(o$sha256) && !identical(o$sha256, unname(hashes[id])))
      .fg_abort("Selected DEM changed between context validation and reference inspection; retry after resolving integrity.")
    if (id %in% blocked) {
      review$files$inspection_status[i] <- "CONTEXT_SELECTION_BLOCKED"
      review$files$reader_note[i] <- "Saved selection was not eligible for inspection"
      review$files$next_action[i] <- "Resolve the saved event selection and file findings; no replacement or CRS conclusion was made."
    }
  }
  # Keep many-event associations and recorded assertions distinct from declarations.
  review$context_links <- links[intersect(c("artifact_id", "survey_event_id", "purpose",
    "evidence", "analyst", "grid_status"), names(links))]
  review$recorded_metadata <- if (length(ids)) inventory[match(ids, inventory$artifact_id),
    c("artifact_id", "vertical_reference", "vertical_unit", "metadata_evidence"), drop = FALSE] else
    data.frame(artifact_id = character(), vertical_reference = character(),
      vertical_unit = character(), metadata_evidence = character())
  review
}
