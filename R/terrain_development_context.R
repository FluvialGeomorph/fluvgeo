# Read-only presentation helpers. This is not a project-persistence binding.
.fg_terrain_visual_context <- function(study_area, streams, reaches, surveys,
    survey_dems, reconstruction, network) {
  hierarchy <- data.frame(entity_type = character(), entity_id = character(),
    parent_id = character(), label = character(), depth = integer())
  node <- function(type, id, parent, label, depth) {
    hierarchy <<- rbind(hierarchy, data.frame(entity_type = type, entity_id = id,
      parent_id = parent, label = label, depth = depth))
  }
  if (!is.null(study_area)) {
    node("Study Area", study_area$study_area_id, NA_character_, study_area$study_area_name, 0L)
  } else if (!is.null(streams) && nrow(streams)) {
    node("Study Area", streams$study_area_id[1], NA_character_, "Study Area name not supplied", 0L)
  }
  if (!is.null(streams)) for (i in seq_len(nrow(streams))) {
    node("Stream", streams$stream_id[i], streams$study_area_id[i], streams$stream_name[i], 1L)
    if (!is.null(reaches)) for (j in which(reaches$stream_id == streams$stream_id[i])) {
      node("Reach", reaches$reach_id[j], reaches$stream_id[j], reaches$reach_name[j], 2L)
      if (nrow(surveys)) for (k in which(surveys$reach_id == reaches$reach_id[j])) {
        node("Survey Event", surveys$survey_event_id[k], surveys$reach_id[k], surveys$date_label[k], 3L)
      }
    }
  }
  if (!is.null(network)) {
    cfg <- network$stream_network_configuration
    obs <- network$stream_network_observation
    node("Configuration", cfg$stream_network_configuration_id, cfg$study_area_id, cfg$configuration_name, 1L)
    obs_date <- .fg_terrain_dates(data.frame(survey_year = obs$observation_year,
      survey_month = obs$observation_month, survey_day = obs$observation_day))
    node("Observation", obs$stream_network_observation_id, cfg$stream_network_configuration_id,
      paste(obs_date, obs$review_status, sep = " / "), 2L)
  }
  # IDs in different entity tables are not assumed globally unique.
  hierarchy$node_key <- paste(hierarchy$entity_type, hierarchy$entity_id, sep = ":")
  parent_type <- c("Study Area" = NA_character_, "Stream" = "Study Area",
    "Reach" = "Stream", "Survey Event" = "Reach", "Configuration" = "Study Area",
    "Observation" = "Configuration")
  hierarchy$parent_key <- ifelse(is.na(hierarchy$parent_id), NA_character_,
    paste(parent_type[hierarchy$entity_type], hierarchy$parent_id, sep = ":"))

  if (is.null(survey_dems)) survey_dems <- list()
  if (!is.list(survey_dems) || (length(survey_dems) &&
      (is.null(names(survey_dems)) || anyNA(names(survey_dems)) ||
       anyDuplicated(names(survey_dems)) || any(!names(survey_dems) %in% surveys$survey_event_id)))) {
    .fg_abort("survey_dems must be uniquely named by supplied Survey Event UUIDs.")
  }
  events <- data.frame(survey_event_id = character(), reach_id = character(),
    reach_label = character(), date_label = character(), evidence_status = character(),
    rows = integer(), columns = integer(), x_cell_size = double(), y_cell_size = double(),
    horizontal_units = character(), horizontal_crs = character(), source_dataset = character())
  extents <- list()
  if (nrow(surveys)) for (i in seq_len(nrow(surveys))) {
    rid <- match(surveys$reach_id[i], reaches$reach_id)
    # Same-named Reaches and same-date events retain separate identities.
    reach_label <- paste(reaches$stream_name[rid], reaches$reach_name[rid], sep = " / ")
    item <- data.frame(survey_event_id = surveys$survey_event_id[i], reach_id = surveys$reach_id[i],
      reach_label = reach_label, date_label = surveys$date_label[i], evidence_status = "INVENTORY_ONLY",
      rows = NA_integer_, columns = NA_integer_, x_cell_size = NA_real_, y_cell_size = NA_real_,
      horizontal_units = NA_character_, horizontal_crs = NA_character_,
      source_dataset = if ("source_dataset" %in% names(surveys)) as.character(surveys$source_dataset[i]) else NA_character_)
    dem <- survey_dems[[surveys$survey_event_id[i]]]
    if (!is.null(dem)) {
      if (!inherits(dem, "SpatRaster") || terra::nlyr(dem) != 1L ||
          !nzchar(terra::crs(dem)) || terra::is.lonlat(dem)) {
        .fg_abort("Each survey DEM must be a single-band projected SpatRaster with a CRS.")
      }
      crs <- sf::st_crs(terra::crs(dem))
      item$evidence_status <- "GRID_SUPPLIED"
      item$rows <- nrow(dem); item$columns <- ncol(dem)
      item$x_cell_size <- terra::res(dem)[1]; item$y_cell_size <- terra::res(dem)[2]
      item$horizontal_units <- crs$units_gdal; item$horizontal_crs <- crs$Name
      bb <- unname(as.vector(terra::ext(dem)))
      extent <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = bb[1], ymin = bb[3],
        xmax = bb[2], ymax = bb[4]), crs = crs)))
      extent$survey_event_id <- item$survey_event_id
      extent$panel <- paste(reach_label, item$date_label, paste0("Event ", i), sep = " | ")
      # Display transform only. Native metadata are retained in event_evidence.
      if (length(extents)) extent <- sf::st_transform(extent, sf::st_crs(extents[[1]]))
      extents[[length(extents) + 1L]] <- extent
    }
    events <- rbind(events, item)
  }

  fields <- c("case_id", "source_ref", "proposed_structure", "evidence", "status", "analyst", "decision_notes")
  if (is.null(reconstruction)) reconstruction <- as.data.frame(stats::setNames(rep(list(character()), length(fields)), fields))
  .fg_require_table(reconstruction, fields, "reconstruction")
  if (inherits(reconstruction, "sf")) .fg_abort("reconstruction must be a nonspatial table.")
  reconstruction <- as.data.frame(reconstruction[fields])
  for (field in fields) {
    if (!is.character(reconstruction[[field]])) .fg_abort("Reconstruction fields must be character columns.")
  }
  for (field in c("case_id", "source_ref", "evidence", "status")) {
    .fg_required_text(reconstruction[[field]], field, nrow(reconstruction))
  }
  if (anyDuplicated(reconstruction$case_id) ||
      any(!reconstruction$status %in% c("PROPOSED", "CONFIRMED", "REJECTED", "UNKNOWN"))) {
    .fg_abort("Reconstruction case IDs must be unique and statuses PROPOSED, CONFIRMED, REJECTED or UNKNOWN.")
  }
  decided <- reconstruction$status %in% c("CONFIRMED", "REJECTED")
  for (field in c("analyst", "decision_notes")) {
    if (any(decided & (is.na(reconstruction[[field]]) | !nzchar(trimws(reconstruction[[field]]))))) {
      .fg_abort("Confirmed/rejected interpretations require analyst and decision_notes.")
    }
  }
  if (any(reconstruction$status %in% c("PROPOSED", "CONFIRMED") &
      (is.na(reconstruction$proposed_structure) | !nzchar(trimws(reconstruction$proposed_structure))))) {
    .fg_abort("Proposed/confirmed interpretations require proposed_structure.")
  }
  assessment <- data.frame(code = character(), entity_id = character(), entity_label = character(), stage = character(),
    status = character(), requires_input = logical(), next_action = character())
  finding <- function(code, id, stage, status, input, action, label) {
    assessment <<- rbind(assessment, data.frame(code = code, entity_id = id,
      entity_label = label, stage = stage, status = status, requires_input = input, next_action = action))
  }
  if (!inherits(study_area, "sf")) finding("STUDY_AOI_NOT_SUPPLIED",
    if (is.null(study_area)) NA_character_ else study_area$study_area_id,
    "STUDY_DEFINITION", "NOT_SUPPLIED", TRUE,
    "Define or recover the intended Study Area AOI; do not substitute a raster rectangle.",
    if (is.null(study_area)) "Study Area not supplied" else study_area$study_area_name)
  if (nrow(events)) for (i in seq_len(nrow(events))) {
    finding("EVENT_TERRAIN_REVIEW", events$survey_event_id[i], "TERRAIN_REVIEW", "NOT_ASSESSED", FALSE,
      if (events$evidence_status[i] == "GRID_SUPPLIED")
        "Grid supplied: assess valid-cell coverage, vertical reference and intended use before scientific comparison." else
        "Inventory only: locate associated terrain if terrain review is required; absence here does not prove loss from the archive.",
      paste(events$reach_label[i], events$date_label[i], paste0("Event ", i), sep = " | "))
  }
  if (nrow(reconstruction)) for (i in seq_len(nrow(reconstruction))) {
    pending <- reconstruction$status[i] %in% c("PROPOSED", "UNKNOWN")
    finding("ARCHIVE_INTERPRETATION", reconstruction$case_id[i], "HIERARCHY_RECONSTRUCTION",
      reconstruction$status[i], pending, if (pending)
        "Review surviving evidence and record the interpretation or unresolved limitation; do not assign governed identities automatically." else
        "Retain the supplied decision and evidence; identity reconciliation and migration validation remain separate.", reconstruction$case_id[i])
  }
  list(hierarchy = hierarchy, event_evidence = events,
    survey_dem_extents = if (length(extents)) do.call(rbind, extents) else NULL,
    reconstruction = reconstruction, assessment = assessment)
}
