#' Review survey opportunities from offline catalog snapshots
#'
#' A read-only single-study assessment. Catalog entries remain distinct from
#' acquisitions and accepted FG sources. No network requests, identity inference,
#' geometry repair, data downloads, or Survey Event changes occur.
#'
#' @param study_area One valid polygon sf row with study_area_id, study_area_name.
#' @param survey_events Data frame with event_id, label, collection_start,
#'   collection_end (Date columns), and date_label. Dates bound known precision;
#'   for a year-only observation use its whole-year interval, not a claimed day.
#' @param catalog_records Polygon sf with catalog, record_id, snapshot_id, title,
#'   collection_start, collection_end (Date), date_label, status and metadata_url.
#'   Status is COMPLETE, PLANNED or UNKNOWN. Optional source_group and
#'   identity_evidence record supplied cross-listing identity, never inferred.
#'   Optional disposition is UNREVIEWED, REPRESENTED, REISSUE or DISMISSED;
#'   reviewed values require review_note, and REPRESENTED/REISSUE require event_id.
#' @param searches Data frame with catalog, snapshot_id, retrieved_at, outcome,
#'   scope_note. Outcome is COMPLETE, PARTIAL, FAILED or NOT_SEARCHED. Completeness
#'   describes this particular query, never the entire world's available surveys.
#' @param focus Optional valid polygon/line sf inside the Study Area. Temporal
#'   comparisons use the supplied event inventory for this explicitly chosen focus.
#' @param focus_label Description of the focus and event-inventory scope.
#' @param terrain_review Optional output of terrain_reference_review(). Adds a
#'   separate evidence snapshot, never source/event identity or acceptance.
#' @return Structured assessment, source rows, map layers and search evidence,
#'   class fg_survey_opportunities. Row classifications are candidate triage,
#'   not scientific comparability or counts of independent acquisitions.
#' @export
survey_opportunity_summary <- function(study_area, survey_events,
    catalog_records, searches, focus = study_area,
    focus_label = "Study Area; supplied event inventory", terrain_review = NULL) {
  if (!is.null(terrain_review) && (!inherits(terrain_review, "fg_terrain_reference_review") ||
      !identical(terrain_review$schema, "TERRAIN_REFERENCE_REVIEW_1")))
    stop("Supply terrain_reference_review() output.", call. = FALSE)
  required <- function(x, fields, what) {
    if (!is.data.frame(x) || !all(fields %in% names(x)))
      stop(what, " requires: ", paste(fields, collapse = ", "), call. = FALSE)
  }
  text_fields <- function(x, fields, what) {
    for (field in fields) if (!is.character(x[[field]]) ||
        anyNA(x[[field]]) || any(!nzchar(trimws(x[[field]]))))
      stop(what, " has missing/invalid ", field, call. = FALSE)
  }
  dates <- function(x, what) {
    for (field in c("collection_start", "collection_end"))
      if (!inherits(x[[field]], "Date") || any(!is.na(x[[field]]) & !is.finite(as.numeric(x[[field]]))))
        stop(what, " dates must be finite Date columns or NA.", call. = FALSE)
    both <- !is.na(x$collection_start) & !is.na(x$collection_end)
    if (any(x$collection_start[both] > x$collection_end[both]))
      stop(what, " date interval is reversed.", call. = FALSE)
  }
  required(study_area, c("study_area_id", "study_area_name"), "Study Area")
  text_fields(study_area, c("study_area_id", "study_area_name"), "Study Area")
  polygon <- function(x) inherits(x, "sf") && !is.na(sf::st_crs(x)) &&
    all(as.character(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))
  if (!polygon(study_area) || nrow(study_area) != 1L ||
      !isTRUE(all(sf::st_is_valid(study_area))) || any(sf::st_is_empty(study_area)))
    stop("Supply one valid nonempty Study Area polygon.", call. = FALSE)
  if (!inherits(focus, "sf") || !nrow(focus) || is.na(sf::st_crs(focus)) ||
      !all(as.character(sf::st_geometry_type(focus)) %in%
        c("POLYGON", "MULTIPOLYGON", "LINESTRING", "MULTILINESTRING")) ||
      !isTRUE(all(sf::st_is_valid(focus))) || any(sf::st_is_empty(focus)))
    stop("Focus must be valid nonempty polygon/line sf.", call. = FALSE)
  focus <- sf::st_transform(focus, sf::st_crs(study_area))
  if (!all(lengths(sf::st_covered_by(focus, study_area)) > 0L))
    stop("Focus must lie inside the Study Area.", call. = FALSE)
  focus_label <- .fg_required_text(focus_label, "focus_label")
  required(survey_events, c("event_id", "label", "collection_start", "collection_end", "date_label"), "Events")
  text_fields(survey_events, c("event_id", "label", "date_label"), "Events")
  dates(survey_events, "Event")
  if (anyDuplicated(survey_events$event_id)) stop("Duplicate event_id.", call. = FALSE)
  required(searches, c("catalog", "snapshot_id", "retrieved_at", "outcome", "scope_note"), "Searches")
  text_fields(searches, names(searches)[names(searches) %in%
    c("catalog", "snapshot_id", "retrieved_at", "outcome", "scope_note")], "Searches")
  search_key <- paste(searches$catalog, searches$snapshot_id, sep = "\r")
  if (anyDuplicated(search_key) || any(!searches$outcome %in%
      c("COMPLETE", "PARTIAL", "FAILED", "NOT_SEARCHED")))
    stop("Search keys must be unique and outcomes valid.", call. = FALSE)
  fields <- c("catalog", "record_id", "snapshot_id", "title", "collection_start",
    "collection_end", "date_label", "status", "metadata_url")
  required(catalog_records, fields, "Catalog")
  if (!polygon(catalog_records)) stop("Catalog must be polygon sf with a CRS.", call. = FALSE)
  text_fields(catalog_records, c("catalog", "record_id", "snapshot_id", "title", "date_label", "status"), "Catalog")
  dates(catalog_records, "Catalog")
  if (anyDuplicated(paste(catalog_records$catalog, catalog_records$record_id, sep = "\r")))
    stop("Duplicate catalog/record_id; choose one snapshot per record.", call. = FALSE)
  if (any(!catalog_records$status %in% c("COMPLETE", "PLANNED", "UNKNOWN")))
    stop("Catalog status must be COMPLETE, PLANNED or UNKNOWN.", call. = FALSE)
  index <- match(paste(catalog_records$catalog, catalog_records$snapshot_id, sep = "\r"), search_key)
  if (anyNA(index) || any(searches$outcome[index] %in% c("FAILED", "NOT_SEARCHED")))
    stop("Catalog records need matching successful or partial search evidence.", call. = FALSE)
  records <- catalog_records
  for (field in c("source_group", "identity_evidence", "review_note", "event_id")) {
    if (!field %in% names(records)) records[[field]] <- rep(NA_character_, nrow(records))
    if (!is.character(records[[field]])) stop(field, " must be character.", call. = FALSE)
  }
  if (!"disposition" %in% names(records)) records$disposition <- rep("UNREVIEWED", nrow(records))
  present <- function(x) !is.na(x) & nzchar(trimws(x))
  if (any(!records$disposition %in% c("UNREVIEWED", "REPRESENTED", "REISSUE", "DISMISSED")) ||
      any(records$disposition != "UNREVIEWED" & !present(records$review_note)))
    stop("Reviewed dispositions require review_note and a valid disposition.", call. = FALSE)
  linked <- records$disposition %in% c("REPRESENTED", "REISSUE")
  if (any(linked & records$status == "PLANNED"))
    stop("Planned records cannot represent an acquired event.", call. = FALSE)
  if (any(linked & !records$event_id %in% survey_events$event_id))
    stop("Represented/reissued records require an existing event_id.", call. = FALSE)
  if (any(present(records$source_group) & !present(records$identity_evidence)))
    stop("source_group requires supplied identity_evidence.", call. = FALSE)
  records$review_id <- sprintf("C%02d", seq_len(nrow(records)))
  records$spatial_relation <- rep("NOT_ASSESSED", nrow(records))
  records$classification <- rep("REVIEW_LOCATION", nrow(records))
  valid <- sf::st_is_valid(records)
  empty <- sf::st_is_empty(records)
  records$spatial_relation[is.na(valid) | !valid] <- "INVALID_GEOMETRY"
  records$spatial_relation[empty] <- "EMPTY_GEOMETRY"
  usable <- which(!is.na(valid) & valid & !empty)
  for (i in usable) {
    relation <- tryCatch({
      shape <- sf::st_transform(records[i, ], sf::st_crs(focus))
      if (!any(lengths(sf::st_intersects(shape, focus)) > 0L)) "OUTSIDE_FOCUS"
      else if (all(lengths(sf::st_covered_by(focus, shape)) > 0L)) "COVERS_FOCUS"
      else "INTERSECTS_FOCUS"
    }, error = function(e) "NOT_ASSESSED")
    records$spatial_relation[i] <- relation
  }
  event_dates_known <- nrow(survey_events) > 0L &&
    !anyNA(survey_events$collection_start) && !anyNA(survey_events$collection_end)
  for (i in seq_len(nrow(records))) {
    s <- records$spatial_relation[i]
    kind <- if (s == "OUTSIDE_FOCUS") "OUTSIDE_FOCUS" else "REVIEW_LOCATION"
    if (s %in% c("COVERS_FOCUS", "INTERSECTS_FOCUS")) {
      kind <- if (records$disposition[i] != "UNREVIEWED") records$disposition[i]
      else if (records$status[i] == "PLANNED") "PLANNED_NOT_ACQUIRED"
      else if (records$status[i] == "UNKNOWN") "REVIEW_AVAILABILITY"
      else if (is.na(records$collection_start[i]) || is.na(records$collection_end[i])) "REVIEW_DATES"
      else if (!event_dates_known) "REVIEW_BASELINE"
      else if (records$collection_end[i] < min(survey_events$collection_start)) "EARLIER_CANDIDATE"
      else if (records$collection_start[i] > max(survey_events$collection_end)) "LATER_CANDIDATE"
      else if (any(records$collection_start[i] <= survey_events$collection_end &
                   records$collection_end[i] >= survey_events$collection_start)) "OVERLAPS_RECORDED_PERIOD"
      else "GAP_CANDIDATE"
    }
    records$classification[i] <- kind
  }
  actions <- c(
    REVIEW_LOCATION = "Resolve missing/invalid footprint; no coverage conclusion.",
    OUTSIDE_FOCUS = "Outside this focus; retain for wider Study Area review.",
    PLANNED_NOT_ACQUIRED = "Future collection only; do not add a Survey Event.",
    REPRESENTED = "Already linked by supplied evidence; no new acquisition inferred.",
    REISSUE = "Revised product of a recorded acquisition; review processing differences.",
    DISMISSED = "Retain prior disposition; no repeat prompt in this snapshot.",
    REVIEW_AVAILABILITY = "Confirm acquisition and product availability.",
    REVIEW_DATES = "Establish acquisition interval from source evidence.",
    REVIEW_BASELINE = "Review against an incomplete/empty existing-event inventory.",
    EARLIER_CANDIDATE = "Check historical source lineage before proposing an earlier observation.",
    LATER_CANDIDATE = "Review source access and comparability for a later observation.",
    OVERLAPS_RECORDED_PERIOD = "Resolve same acquisition versus independent survey; do not count twice.",
    GAP_CANDIDATE = "Review source lineage and comparability for an intervening observation.")
  records$next_action <- unname(actions[records$classification])
  records$search_outcome <- searches$outcome[index]
  structure(list(schema = "SURVEY_OPPORTUNITIES_1", study_area = study_area,
    focus = focus, focus_label = focus_label, survey_events = survey_events,
    records = records, searches = searches, terrain_review = terrain_review,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    class = "fg_survey_opportunities")
}

#' Render a durable survey-opportunity report
#' @param summary Output of survey_opportunity_summary().
#' @param output_file New HTML path in an existing local directory. Never replaced.
#' @return Normalized report path, invisibly. Requires knitr, gt, Pandoc and local
#'   hard-link support. Essential maps/timeline are embedded; no online basemap.
#' @export
survey_opportunity_report <- function(summary, output_file) {
  if (!inherits(summary, "fg_survey_opportunities") ||
      !identical(summary$schema, "SURVEY_OPPORTUNITIES_1"))
    stop("Supply survey_opportunity_summary() output.", call. = FALSE)
  output_file <- .fg_required_text(output_file, "output_file")
  if (!grepl("\\.html$", output_file, ignore.case = TRUE) || !dir.exists(dirname(output_file)))
    stop("Supply a new .html path in an existing directory.", call. = FALSE)
  output_file <- file.path(normalizePath(dirname(output_file), winslash = "/"), basename(output_file))
  if (file.exists(output_file)) stop("Report destination already exists.", call. = FALSE)
  if (!requireNamespace("knitr", quietly = TRUE) || !rmarkdown::pandoc_available())
    stop("Rendering requires knitr and Pandoc.", call. = FALSE)
  stage <- tempfile("survey-report-", tmpdir = dirname(output_file), fileext = ".html")
  on.exit(unlink(stage), add = TRUE)
  rmarkdown::render(system.file("reports", "survey_opportunity_report.Rmd", package = "fluvgeo"),
    output_file = stage, intermediates_dir = tempdir(), params = list(report = summary,
      terrain_template = system.file("reports", "terrain_reference_review.Rmd", package = "fluvgeo")),
    envir = .fg_report_environment(), quiet = TRUE)
  if (!isTRUE(suppressWarnings(file.link(stage, output_file))))
    stop("Could not publish without replacement; use a hard-link-capable local filesystem.", call. = FALSE)
  invisible(output_file)
}
