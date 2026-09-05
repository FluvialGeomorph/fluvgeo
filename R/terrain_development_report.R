#' Assemble a pre-Level-1 Terrain Development review
#'
#' Read-only report data for R and future Shiny clients. Missing inputs remain
#' explicit; no hierarchy, Survey Event, acceptance, or Level 1 readiness is
#' inferred. Context tables are report inputs, not new FGDB persistence relations.
#'
#' @param study_area Optional one-row polygon sf with study_area_id and
#'   study_area_name. Its analyst-defined AOI is never inferred from a DEM extent.
#' @param streams Optional data frame with stream_id, study_area_id, stream_name.
#'   These are selected Streams; defaults to network Configuration membership.
#' @param reaches Optional data frame with reach_id, stream_id, reach_name.
#'   Optional sf geometry represents polygon AOIs, not flowlines.
#' @param survey_events Optional data frame with survey_event_id, reach_id,
#'   survey_year and optional survey_month/survey_day. This inventory does not
#'   verify file availability or network-to-event associations.
#' @param network Optional Stream Network relation list or fluvgeo GeoPackage path.
#' @param dem Optional single-band projected terra SpatRaster. This first slice
#'   displays its extent/grid metadata, not an elevation preview or valid-cell mask.
#' @param terrain_notes Supplied terrain source, processing, units and limitations.
#' @param analyst_notes Supplied scope, segmentation rationale and next decisions.
#' @return List of tables, map layers, fresh network validation and explicit gaps.
#'   Inputs and saved network history remain unchanged.
#' @export
terrain_development_summary <- function(study_area = NULL, streams = NULL,
    reaches = NULL, survey_events = NULL, network = NULL, dem = NULL,
    terrain_notes = NA_character_, analyst_notes = NA_character_) {
  terrain_notes <- .fg_optional_text(terrain_notes, "terrain_notes")
  analyst_notes <- .fg_optional_text(analyst_notes, "analyst_notes")
  if (is.character(network)) network <- read_stream_network_geodatabase(network, validate = FALSE)
  if (!is.null(network)) .fg_check_network_bundle(network)
  gaps <- character()
  add <- function(s) gaps <<- c(gaps, s)
  if (!is.null(study_area)) {
    .fg_terrain_context(study_area, "study_area_id", "study_area_name")
    .fg_terrain_polygon(study_area)
    if (nrow(study_area) != 1L) .fg_abort("Supply exactly one Study Area AOI.")
  } else add("Study Area AOI not supplied; no boundary has been inferred.")
  if (is.null(streams) && !is.null(network)) {
    streams <- network$stream_network_configuration_stream[c("stream_id", "stream_name")]
    streams$study_area_id <- network$stream_network_configuration$study_area_id
    add("Selected Streams come from this network Configuration, not a complete Study Area inventory.")
  }
  if (!is.null(streams)) {
    .fg_terrain_context(streams, "stream_id", "stream_name", "study_area_id")
    if (inherits(streams, "sf")) .fg_terrain_polygon(streams)
    if (length(unique(streams$study_area_id)) > 1L) .fg_abort("Selected Streams must belong to one Study Area.")
    if (!is.null(study_area) && any(streams$study_area_id != study_area$study_area_id)) .fg_abort("Stream Study Area ownership mismatch.")
  } else add("Selected Streams not supplied.")
  if (!is.null(reaches)) {
    .fg_terrain_context(reaches, "reach_id", "reach_name", "stream_id")
    if (inherits(reaches, "sf")) .fg_terrain_polygon(reaches)
    if (is.null(streams) || any(!reaches$stream_id %in% streams$stream_id)) .fg_abort("Reaches require their selected parent Streams.")
    reaches$stream_name <- streams$stream_name[match(reaches$stream_id, streams$stream_id)]
  } else add("Reach definitions not supplied; segmentation has not been inferred.")
  surveys <- data.frame()
  if (!is.null(survey_events)) {
    .fg_terrain_context(survey_events, "survey_event_id", parent = "reach_id")
    if (is.null(reaches) || any(!survey_events$reach_id %in% reaches$reach_id)) .fg_abort("Survey Events require their parent Reaches.")
    surveys <- sf::st_drop_geometry(survey_events)
    .fg_require_table(surveys, "survey_year", "Survey Events")
    for (nm in c("survey_month", "survey_day")) if (!nm %in% names(surveys)) surveys[[nm]] <- rep(NA_integer_, nrow(surveys))
    surveys$date_label <- .fg_terrain_dates(surveys)
    surveys$reach_name <- reaches$reach_name[match(surveys$reach_id, reaches$reach_id)]
    add("Survey Event inventory is supplied context; terrain-file availability and network-to-event associations are not verified.")
  } else add("Survey Event inventory not supplied; the network observation date is not substituted.")
  validation <- NULL
  segments <- NULL
  observation <- data.frame()
  if (!is.null(network)) {
    cid <- network$stream_network_configuration$study_area_id
    if ((!is.null(study_area) && cid != study_area$study_area_id) ||
        (!is.null(streams) && any(streams$study_area_id != cid))) .fg_abort("Network Study Area ownership mismatch.")
    segments <- network$stream_network
    if (any(!segments$stream_id %in% streams$stream_id)) .fg_abort("Network segments reference Streams absent from report context.")
    if (!is.null(reaches)) {
      mapped <- which(!is.na(segments$reach_id))
      if (any(!segments$reach_id[mapped] %in% reaches$reach_id) ||
          any(reaches$stream_id[match(segments$reach_id[mapped], reaches$reach_id)] != segments$stream_id[mapped])) .fg_abort("Network Reach classifications disagree with report context.")
    }
    observation <- network$stream_network_observation
    level <- if (observation$review_status == "ACCEPTED") "ACCEPTANCE" else "WORKING"
    validation <- .fg_validate_network_bundle(network, level, "terrain-development-report", reaches)
    if (observation$review_status == "ACCEPTED") {
      tryCatch(.fg_check_accepted_bundle(network, reaches), error = function(e) {
        add(paste("Stored acceptance cannot be confirmed:", conditionMessage(e)))
      })
    }
    if (validation$stream_network_validation_run$result != "PASS") add("Current network checks require attention; see the fresh findings below.")
    if (observation$review_status != "ACCEPTED") add("Network Observation is not accepted.")
    if (anyNA(segments$reach_id)) add("Some network segments have no Reach assignment; the map shows these explicitly.")
    if (observation$coverage_status != "FULL_CONFIGURATION") add(paste("Network coverage:", observation$coverage_status))
    if (observation$provenance_completeness != "COMPLETE") add(paste("Network provenance:", observation$provenance_completeness))
    segments$report_reach <- paste(streams$stream_name[match(segments$stream_id, streams$stream_id)], ifelse(is.na(segments$reach_id), "Reach unassigned",
      if (is.null(reaches)) segments$reach_id else reaches$reach_name[match(segments$reach_id, reaches$reach_id)])
      , sep = " / ")
  } else add("No Stream Network Observation supplied; this is a scope-definition report.")
  terrain <- data.frame()
  footprint <- NULL
  if (!is.null(dem)) {
    if (!inherits(dem, "SpatRaster") || terra::nlyr(dem) != 1L ||
        !nzchar(terra::crs(dem)) || terra::is.lonlat(dem)) .fg_abort("DEM must be a single-band projected SpatRaster with a CRS.")
    bb <- unname(as.vector(terra::ext(dem)))
    footprint <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = bb[1], ymin = bb[3], xmax = bb[2], ymax = bb[4]), crs = sf::st_crs(terra::crs(dem)))))
    terrain <- data.frame(property = c("Rows", "Columns", "X cell size", "Y cell size", "Horizontal units", "CRS", "Footprint meaning"),
      value = c(nrow(dem), ncol(dem), terra::res(dem), sf::st_crs(footprint)$units_gdal,
                sf::st_crs(footprint)$Name, "Raster extent only; internal NoData and valid-cell coverage not assessed"))
    if (!is.null(segments) && any(lengths(sf::st_covered_by(sf::st_transform(segments, sf::st_crs(footprint)), footprint)) == 0L)) {
      add("Some network geometry extends outside the supplied DEM extent. This DEM cannot cover the whole supplied network.")
    }
    add("DEM extent agreement does not verify valid-cell coverage or historical derivation provenance.")
  } else add("DEM not supplied; terrain extent and grid metadata are unavailable.")
  if (is.na(terrain_notes)) add("Terrain source, processing history, vertical units/datum and qualifications have not been described.")
  if (is.na(analyst_notes)) add("Analyst scope and segmentation rationale have not been supplied.")
  list(study_area = study_area, streams = streams, reaches = reaches, surveys = surveys,
    observation = observation, segments = segments, dem_extent = footprint, terrain = terrain,
    validation = validation, gaps = unique(gaps), terrain_notes = terrain_notes, analyst_notes = analyst_notes,
    generated_at = Sys.time(), schema = "TERRAIN_DEVELOPMENT_REPORT_1")
}

.fg_terrain_context <- function(x, id, label = NULL, parent = NULL) {
  .fg_require_table(x, c(id, label, parent), "report context")
  for (field in c(id, parent)) if (nrow(x)) {
    canonical <- .fg_uuid(x[[field]], field)
    if (!identical(unname(x[[field]]), canonical)) .fg_abort("Report identities must use canonical UUID text.")
  }
  if (anyDuplicated(x[[id]])) .fg_abort("Report context identities must be unique.")
  if (!is.null(label)) .fg_required_text(x[[label]], label, nrow(x))
}

.fg_terrain_polygon <- function(x) {
  if (!inherits(x, "sf") || is.na(sf::st_crs(x)) ||
      any(!sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")) ||
      any(sf::st_is_empty(x)) || !isTRUE(all(sf::st_is_valid(x)))) .fg_abort("Context AOIs must be valid, nonempty polygon sf with a CRS.")
}

.fg_terrain_dates <- function(x) {
  out <- character(nrow(x))
  for (i in seq_len(nrow(x))) {
    y <- x$survey_year[i]; m <- x$survey_month[i]; d <- x$survey_day[i]
    if (!is.numeric(y) || is.na(y) || !is.finite(y) || y %% 1 != 0 || y < 1000 || y > 9999) .fg_abort("Survey Event year must be a four-digit integer.")
    if (!is.na(m) && (!is.numeric(m) || !is.finite(m) || m %% 1 != 0 || m < 1 || m > 12)) .fg_abort("Invalid Survey Event month.")
    if (!is.na(d) && (is.na(m) || !is.numeric(d) || !is.finite(d) || d %% 1 != 0 || d < 1 || d > 31)) .fg_abort("Invalid Survey Event day or missing month.")
    out[i] <- if (is.na(m)) sprintf("%04d", as.integer(y)) else if (is.na(d)) sprintf("%04d-%02d", as.integer(y), as.integer(m)) else sprintf("%04d-%02d-%02d", as.integer(y), as.integer(m), as.integer(d))
    if (!is.na(d) && (is.na(as.Date(out[i])) || format(as.Date(out[i]), "%Y-%m-%d") != out[i])) .fg_abort("Invalid Survey Event calendar date.")
  }
  out
}

#' Render an offline Terrain Development HTML report
#'
#' Uses only supplied local data; no ArcPy, ArcGIS client, basemap service, or
#' credentials are required for this reporting path. This does not imply all
#' terrain derivation is implemented in fluvgeo. The report is not an acceptance
#' action or a Level 1 readiness certificate.
#'
#' @param summary Output of terrain_development_summary().
#' @param output_file New .html file in an existing directory; never overwritten.
#' @return Normalized report path, invisibly. Requires Pandoc and knitr. Publication
#'   requires a local hard-link-capable filesystem, failing safely otherwise.
#' @export
terrain_development_report <- function(summary, output_file) {
  if (!is.list(summary) || !identical(summary$schema, "TERRAIN_DEVELOPMENT_REPORT_1")) .fg_abort("Supply a terrain_development_summary() result.")
  output_file <- .fg_required_text(output_file, "output_file")
  if (!grepl("\\.html$", output_file, ignore.case = TRUE) || !dir.exists(dirname(output_file))) .fg_abort("Supply a new .html path in an existing directory.")
  output_file <- file.path(normalizePath(dirname(output_file), winslash = "/"), basename(output_file))
  if (file.exists(output_file)) .fg_abort("Report destination already exists.")
  if (!requireNamespace("knitr", quietly = TRUE) || !rmarkdown::pandoc_available()) .fg_abort("Rendering requires knitr and Pandoc.")
  stage <- tempfile("terrain-report-", tmpdir = dirname(output_file), fileext = ".html")
  on.exit(unlink(stage), add = TRUE)
  template <- system.file("reports", "terrain_development_report.Rmd", package = "fluvgeo")
  rmarkdown::render(template, output_file = stage, intermediates_dir = tempdir(),
    params = list(report = summary), envir = new.env(parent = baseenv()), quiet = TRUE)
  if (!isTRUE(suppressWarnings(file.link(stage, output_file)))) .fg_abort("Could not publish report without replacement; use a local hard-link-capable filesystem.")
  invisible(output_file)
}
