#' Apply explicit segment-role classifications to prepared candidates
#'
#' Applies UUID-keyed role decisions to a copy and records CLASSIFY_SEGMENT_ROLE
#' operations with actor, time, and decision notes. No role is inferred from
#' graph degree, length, or feature order. Review decisions, validation history,
#' geometry, node identities, and source lineage are unchanged. Reapplying an
#' unchanged role is a no-op. Accepted segments must be reopened before changes.
#'
#' @param prepared Named result of prepare_stream_network_from_features().
#' @param classifications Data frame with unique `stream_network_segment_id`,
#'   `segment_role` (MAINSTEM, TRIBUTARY, CONNECTOR, ARTIFICIAL), and nonblank
#'   `decision_notes`. May cover a subset, but cannot contain foreign IDs.
#' @param actor Identifier of the person/process supplying the decisions.
#' @param performed_at Operation time, converted to UTC.
#' @return A copy of prepared with updated stream_network and appended
#'   stream_network_operation rows. Does not accept inspection features or the
#'   observation; call validate_stream_network() on the current state afterward.
#' @export
classify_stream_network_segments <- function(
    prepared, classifications, actor, performed_at = Sys.time()) {
  x <- prepared$stream_network
  operations <- prepared$stream_network_operation
  sources <- prepared$stream_network_source
  .fg_require_table(x, c("stream_network_segment_id", "segment_role", "review_status",
                        "modified_at", "modified_by"), "prepared$stream_network")
  if (!inherits(x, "sf") || !nrow(x)) .fg_abort("Prepared segments must be nonempty sf.")
  .fg_require_table(classifications, c("stream_network_segment_id", "segment_role", "decision_notes"), "classifications")
  .fg_require_table(operations, c("stream_network_operation_id", "stream_network_segment_id",
    "stream_network_source_id", "operation_sequence", "operation_code", "operation_notes",
    "performed_at", "performed_by"), "stream_network_operation")
  .fg_require_table(sources, c("stream_network_source_id", "stream_network_segment_id"), "stream_network_source")
  actor <- .fg_required_text(actor, "actor")
  performed_at <- .fg_timestamp(performed_at, "performed_at")
  ids <- .fg_uuid(x$stream_network_segment_id, "segment IDs")
  if (anyDuplicated(ids)) .fg_abort("Prepared segment IDs must be unique.")
  if (!nrow(classifications)) return(prepared)
  selected <- .fg_uuid(classifications$stream_network_segment_id, "classification segment IDs")
  if (anyDuplicated(selected) || any(!selected %in% ids)) {
    .fg_abort("Classification IDs must be unique and belong to the prepared segments.")
  }
  roles <- as.character(classifications$segment_role)
  if (anyNA(roles) || any(!roles %in% c("MAINSTEM", "TRIBUTARY", "CONNECTOR", "ARTIFICIAL"))) {
    .fg_abort("Classification roles must be MAINSTEM, TRIBUTARY, CONNECTOR, or ARTIFICIAL.")
  }
  notes <- .fg_required_text(classifications$decision_notes, "decision_notes", length(selected))
  if (anyNA(operations$operation_sequence) || !is.numeric(operations$operation_sequence) ||
      any(!is.finite(operations$operation_sequence) | operations$operation_sequence < 1 | operations$operation_sequence %% 1 != 0) ||
      anyDuplicated(operations[c("stream_network_segment_id", "operation_sequence")])) {
    .fg_abort("Existing operation sequences must be unique positive integers per segment.")
  }
  rows <- match(selected, ids)
  changed <- is.na(x$segment_role[rows]) | as.character(x$segment_role[rows]) != roles
  if (any(x$review_status[rows[changed]] %in% "ACCEPTED")) {
    .fg_abort("Reopen accepted segments before changing their roles.")
  }
  rows <- rows[changed]
  selected <- selected[changed]
  if (!length(rows)) return(prepared)
  x$segment_role <- as.character(x$segment_role)
  x$segment_role[rows] <- roles[changed]
  x$modified_at[rows] <- performed_at
  x$modified_by[rows] <- actor
  applied <- operations[rep(NA_integer_, length(rows)), ]
  applied$stream_network_operation_id <- .fg_generate_uuid(length(rows))
  applied$stream_network_segment_id <- selected
  applied$stream_network_source_id <- vapply(selected, function(id) {
    matches <- sources$stream_network_source_id[sources$stream_network_segment_id == id]
    if (length(matches) == 1L) matches else NA_character_
  }, character(1), USE.NAMES = FALSE)
  applied$operation_sequence <- vapply(selected, function(id) {
    as.integer(max(c(0L, operations$operation_sequence[operations$stream_network_segment_id == id]))) + 1L
  }, integer(1), USE.NAMES = FALSE)
  applied$operation_code <- "CLASSIFY_SEGMENT_ROLE"
  applied$segment_role <- roles[changed]
  applied$operation_notes <- paste0("Explicit role ", roles[changed], ": ", notes[changed])
  applied$performed_at <- performed_at
  applied$performed_by <- actor
  prepared$stream_network <- x
  prepared$stream_network_operation <- dplyr::bind_rows(operations, applied)
  prepared
}

.fg_require_table <- function(x, fields, name) {
  if (!is.data.frame(x) || !all(fields %in% names(x))) {
    .fg_abort(paste0("`", name, "` must be a data frame containing: ", paste(fields, collapse = ", "), "."))
  }
}
