#' Accept an explicitly reviewed retained Stream Network
#'
#' Operates on a named list of local relations, not on a database connection.
#' Supply inspection decisions before calling: this function never fills them
#' in. Current ACCEPTANCE validation must pass. Failed validation raises a
#' `fluvgeo_acceptance_error` carrying `validation` and `geodatabase` (the unchanged
#' scientific state with the new run appended). Nothing is written to disk.
#'
#' Successful acceptance records observation reviewer/time/notes and sets all
#' segment review states to ACCEPTED. Segment modified times describe the
#' inspected scientific state and are deliberately unchanged by this status-only
#' transition. Observation modification provenance records the transition.
#' Existing validation history and inspection decisions are preserved. Already
#' accepted observations must be explicitly reopened before a new acceptance.
#'
#' @param geodatabase Named list containing Configuration, membership,
#'   Observation, prepared segments, and their related evidence tables.
#' @param reviewer Explicit observation reviewer's identifier.
#' @param review_notes Optional replacement qualification notes. NA preserves
#'   existing notes; missing scientific qualifications are never invented.
#' @param reaches Optional governed Reach-Stream mappings for validation.
#' @param accepted_at Acceptance time, converted to UTC; cannot predate inspections.
#' @return A new relations list, with an appended passing acceptance run and
#'   accepted observation/segment status. Input objects remain unchanged.
#' @export
accept_stream_network <- function(geodatabase, reviewer, review_notes = NA_character_,
                                  reaches = NULL, accepted_at = Sys.time()) {
  .fg_check_network_bundle(geodatabase)
  reviewer <- .fg_required_text(reviewer, "reviewer")
  review_notes <- .fg_optional_text(review_notes, "review_notes")
  accepted_at <- .fg_timestamp(accepted_at, "accepted_at")
  out <- geodatabase
  obs <- out$stream_network_observation
  if (!obs$review_status %in% c("DRAFT", "READY_FOR_REVIEW")) {
    .fg_abort("Only DRAFT or READY_FOR_REVIEW observations can be accepted; explicitly reopen other states.")
  }
  if (!is.na(review_notes)) obs$review_notes <- review_notes
  out$stream_network_observation <- obs
  checked <- .fg_validate_network_bundle(out, "ACCEPTANCE", reviewer, reaches, accepted_at)
  # Do not allow a future-dated inspection to authorize an earlier transition.
  times <- c(out$stream_network_review$decision_at, out$stream_network$modified_at,
             obs$modified_at, out$stream_network_operation$performed_at)
  if (any(times > accepted_at, na.rm = TRUE)) {
    .fg_abort("accepted_at cannot predate inspections, operations, or current modifications.")
  }
  for (nm in names(checked)) out[[nm]] <- dplyr::bind_rows(out[[nm]], checked[[nm]])
  attr(out, "validation") <- checked
  if (checked$stream_network_validation_run$result != "PASS") {
    # Proposed observation notes are also uncommitted when the transition fails.
    out$stream_network_observation <- geodatabase$stream_network_observation
    stop(structure(list(message = paste("Stream Network acceptance blocked:",
      paste(unique(checked$stream_network_validation_issue$issue_code), collapse = ", ")),
      call = NULL, validation = checked, geodatabase = out),
      class = c("fluvgeo_acceptance_error", "error", "condition")))
  }
  obs$review_status <- "ACCEPTED"
  obs$reviewed_at <- obs$modified_at <- accepted_at
  obs$reviewed_by <- obs$modified_by <- reviewer
  out$stream_network_observation <- obs
  out$stream_network$review_status <- "ACCEPTED"
  out
}

.fg_validate_network_bundle <- function(x, level, actor, reaches = NULL, at = Sys.time()) {
  validate_stream_network(x$stream_network_configuration, x$stream_network_configuration_stream,
    x$stream_network_observation, x$stream_network, sources = x$stream_network_source,
    operations = x$stream_network_operation, nodes = x$stream_network_node,
    connections = x$stream_network_connection, review_features = x$stream_network_review,
    reaches = reaches, level = level, actor = actor, validated_at = at)
}

.fg_network_relation_names <- function() c("stream_network_configuration",
  "stream_network_configuration_stream", "stream_network_observation", "stream_network",
  "stream_network_source", "stream_network_operation", "stream_network_direction_evidence",
  "stream_network_review", "stream_network_node", "stream_network_connection",
  "stream_network_validation_run", "stream_network_validation_issue")

# Container integrity is independent of scientific readiness: unresolved drafts
# may be saved, but broken evidence references must not silently travel with them.
.fg_check_network_bundle <- function(x) {
  required <- c("stream_network_configuration", "stream_network_configuration_stream",
    "stream_network_observation", "stream_network", "stream_network_validation_run",
    "stream_network_validation_issue")
  if (!is.list(x) || is.data.frame(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !all(required %in% names(x)) || any(!names(x) %in% .fg_network_relation_names()) ||
      !all(vapply(x, is.data.frame, logical(1)))) {
    .fg_abort("Supply uniquely named Stream Network relations, including configuration, membership, observation, segments, and validation history.")
  }
  for (nm in required[1:3]) {
    if (nrow(x[[nm]]) < 1L || (nm != required[2] && nrow(x[[nm]]) != 1L)) {
      .fg_abort("A local bundle requires one Configuration, one Observation, and nonempty membership.")
    }
  }
  keys <- c(stream_network_configuration = "stream_network_configuration_id",
    stream_network_observation = "stream_network_observation_id", stream_network = "stream_network_segment_id",
    stream_network_source = "stream_network_source_id", stream_network_operation = "stream_network_operation_id",
    stream_network_direction_evidence = "stream_network_segment_id", stream_network_review = "stream_network_review_id",
    stream_network_node = "node_id", stream_network_validation_run = "stream_network_validation_run_id",
    stream_network_validation_issue = "stream_network_validation_issue_id")
  for (nm in intersect(names(keys), names(x))) {
    .fg_require_table(x[[nm]], keys[[nm]], nm)
    id <- x[[nm]][[keys[[nm]]]]
    if (length(id)) .fg_uuid(id, paste(nm, "IDs"))
    if (anyDuplicated(id)) .fg_abort(paste("Duplicate IDs in", nm))
  }
  fk <- function(table, field, target, key, nullable = FALSE) {
    if (!table %in% names(x)) return(invisible(NULL))
    .fg_require_table(x[[table]], field, table)
    values <- x[[table]][[field]]
    if ((!nullable && anyNA(values)) || any(!is.na(values) & !values %in% x[[target]][[key]])) {
      .fg_abort(paste("Broken reference:", table, field))
    }
  }
  for (nm in c("stream_network_configuration_stream", "stream_network_observation")) {
    fk(nm, "stream_network_configuration_id", "stream_network_configuration", "stream_network_configuration_id")
  }
  for (nm in c("stream_network", "stream_network_review", "stream_network_node",
               "stream_network_connection", "stream_network_validation_run")) {
    fk(nm, "stream_network_observation_id", "stream_network_observation", "stream_network_observation_id")
  }
  for (nm in c("stream_network_source", "stream_network_operation", "stream_network_direction_evidence", "stream_network_review", "stream_network_connection")) {
    fk(nm, "stream_network_segment_id", "stream_network", "stream_network_segment_id")
  }
  fk("stream_network_validation_issue", "stream_network_validation_run_id", "stream_network_validation_run", "stream_network_validation_run_id")
  fk("stream_network_review", "stream_network_validation_issue_id", "stream_network_validation_issue", "stream_network_validation_issue_id")
  for (nm in c("stream_network_review", "stream_network_operation")) {
    fk(nm, "stream_network_source_id", "stream_network_source", "stream_network_source_id", TRUE)
  }
  fk("stream_network_direction_evidence", "stream_network_operation_id", "stream_network_operation", "stream_network_operation_id", TRUE)
  invisible(x)
}
