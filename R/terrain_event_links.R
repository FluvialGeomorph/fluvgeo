# Explicit local associations, not an FGDB identity or terrain-edition binding.
.fg_terrain_event_links <- function(links, artifact_ids, formats) {
  fields <- c("artifact_id", "survey_event_id", "purpose", "evidence", "analyst", "use_for_report")
  empty <- data.frame(artifact_id = character(), survey_event_id = character(),
    purpose = character(), evidence = character(), analyst = character(), use_for_report = logical())
  if (is.null(links)) return(empty)
  .fg_require_table(links, fields, "event_links")
  if (inherits(links, "sf") || !nrow(links)) .fg_abort("event_links must be a nonempty nonspatial table.")
  links <- as.data.frame(links[fields], stringsAsFactors = FALSE)
  for (field in fields[fields != "use_for_report"])
    .fg_required_text(links[[field]], field, nrow(links))
  if (!identical(unname(links$survey_event_id), .fg_uuid(links$survey_event_id, "survey_event_id")))
    .fg_abort("Event links require canonical Survey Event UUIDs.")
  if (!is.logical(links$use_for_report) || anyNA(links$use_for_report))
    .fg_abort("use_for_report must be nonmissing logical values.")
  if (any(!links$artifact_id %in% artifact_ids)) .fg_abort("Event links reference an unknown artifact.")
  if (anyDuplicated(links[c("artifact_id", "survey_event_id")]))
    .fg_abort("Each artifact/event pair must be unique.")
  if (anyDuplicated(links$survey_event_id[links$use_for_report]))
    .fg_abort("Select at most one report DEM per Survey Event.")
  if (any(formats[match(links$artifact_id[links$use_for_report], artifact_ids)] != "GeoTIFF"))
    .fg_abort("A report DEM must reference a GeoTIFF artifact.")
  rownames(links) <- NULL
  links
}

.fg_terrain_resolve_events <- function(folder, manifest, surveys, survey_dems) {
  links <- folder$event_links
  assessment <- folder$assessment[FALSE, , drop = FALSE]
  if (is.null(links)) return(list(survey_dems = survey_dems,
    event_artifacts = NULL, assessment = assessment))
  if (is.null(survey_dems)) survey_dems <- list()
  if (!is.list(survey_dems)) .fg_abort("survey_dems must be a named list.")
  selected <- links$survey_event_id[links$use_for_report]
  if (any(selected %in% names(survey_dems)))
    .fg_abort("A Survey Event has both a manifest-selected DEM and survey_dems; choose one source.")
  artifacts <- folder$artifacts
  links$path <- artifacts$path[match(links$artifact_id, artifacts$artifact_id)]
  links$event_in_context <- links$survey_event_id %in% surveys$survey_event_id
  links$grid_status <- ifelse(links$use_for_report, "NOT_LOADED", "NOT_SELECTED")
  root <- .fg_manifest_root(dirname(normalizePath(manifest, winslash = "/", mustWork = TRUE)))
  finding <- function(i, code, status, input, action) {
    assessment <<- rbind(assessment, data.frame(code = code, entity_id = links$survey_event_id[i],
      entity_label = paste(links$survey_event_id[i], links$path[i], sep = " | "),
      stage = "EVENT_ASSOCIATION", status = status, requires_input = input, next_action = action))
  }
  blocked <- folder$assessment$entity_id[folder$assessment$status == "BLOCKED"]
  for (i in seq_len(nrow(links))) {
    if (!links$event_in_context[i]) {
      finding(i, "EVENT_CONTEXT_MISSING", "REVIEW_REQUIRED", TRUE,
        "Supply or reconcile the referenced Survey Event and its parent context; do not infer it from the filename.")
      next
    }
    if (!links$use_for_report[i]) next
    a <- match(links$artifact_id[i], artifacts$artifact_id)
    if (!artifacts$hash_verified[a] || links$artifact_id[i] %in% blocked) {
      finding(i, "EVENT_DEM_BLOCKED", "BLOCKED", FALSE,
        "Resolve the linked file integrity or metadata finding before using its grid; no substitute was selected.")
      next
    }
    path <- .fg_manifest_path(root, links$path[i])
    dem <- tryCatch(terra::rast(path, opts = "GEOREF_SOURCES=INTERNAL"), error = function(e) NULL)
    if (is.null(dem) || terra::nlyr(dem) != 1L || !nzchar(terra::crs(dem)) || terra::is.lonlat(dem)) {
      finding(i, "EVENT_DEM_UNSUPPORTED", "REVIEW_REQUIRED", TRUE,
        "The report grid view requires a readable single-band projected DEM with a CRS; review the linked source without silently reprojecting it.")
      next
    }
    survey_dems[[links$survey_event_id[i]]] <- dem
    links$grid_status[i] <- "GRID_LOADED"
  }
  list(survey_dems = survey_dems, event_artifacts = links, assessment = assessment)
}
