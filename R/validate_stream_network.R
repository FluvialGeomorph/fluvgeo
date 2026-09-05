#' Validate current retained-network state before observation acceptance
#'
#' Read-only local validation: recomputes geometry findings and hydroloom-backed
#' connectivity, compares persisted node/connection tables, checks classified
#' roles, membership, lineage references and operation order. Earlier validation
#' results are not trusted or overwritten. This first implementation supports
#' SOURCE_NETWORK_RETAINED observations only.
#'
#' ACCEPTANCE additionally requires explicit accepted inspection decisions and
#' their actor/time/notes, and observation review notes qualifying incomplete
#' coverage/provenance or disconnected/multiple-outlet topology. PASS means these
#' local checks passed, not that the observation was accepted. No review status
#' is changed. Terrain provenance, scientific suitability of supplied roles,
#' undeclared boundaries, source completeness, and enterprise identity still
#' require human/loader checks. Malformed tables/UUIDs raise input errors;
#' assessed inconsistencies return blocking findings.
#'
#' @param configuration One governed Configuration row.
#' @param configuration_streams Configuration-Stream membership table.
#' @param observation One constructor-compatible Observation row.
#' @param stream_network Current candidate segment sf.
#' @param sources Source lineage table; required at ACCEPTANCE.
#' @param operations Applied operation table; required at ACCEPTANCE (may be empty).
#' @param level WORKING or ACCEPTANCE.
#' @param nodes Current stream_network_node POINT sf.
#' @param connections Current stream_network_connection table.
#' @param review_features Current stream_network_review sf. Required at ACCEPTANCE;
#'   every candidate must have at least one accepted inspection and no pending or
#'   rejected required inspection. INSPECT acceptance does not authorize repairs.
#' @param reaches Optional table of unique reach_id and stream_id, required to
#'   verify any nonmissing candidate Reach classifications.
#' @param actor Validator's actor/process identifier (not an inferred reviewer).
#' @param validated_at Validation time, converted to UTC.
#' @return Named validation run and issue tibbles, with result PASS or
#'   REVIEW_REQUIRED. A new run UUID is generated; inputs and old runs are unchanged.
#' @export
validate_stream_network <- function(
    configuration, configuration_streams, observation, stream_network,
    sources = NULL, operations = NULL, level = c("WORKING", "ACCEPTANCE"),
    nodes = NULL, connections = NULL, review_features = NULL, reaches = NULL,
    actor, validated_at = Sys.time()) {
  level <- match.arg(level)
  actor <- .fg_required_text(actor, "actor")
  validated_at <- .fg_timestamp(validated_at, "validated_at")
  .fg_require_table(configuration, c("stream_network_configuration_id", "configuration_mode"), "configuration")
  .fg_require_table(configuration_streams, c("stream_network_configuration_id", "stream_id"), "configuration_streams")
  .fg_require_table(observation, c("stream_network_observation_id", "stream_network_configuration_id",
    "evidence_class", "coverage_status", "provenance_completeness", "review_notes",
    "topology_tolerance", "topology_tolerance_unit", "native_horizontal_crs", "review_status", "lifecycle_status"), "observation")
  .fg_require_table(stream_network, c("stream_network_segment_id", "stream_network_observation_id",
    "stream_id", "reach_id", "segment_role", "direction_status", "direction_method", "upstream_node_id", "downstream_node_id", "modified_at", "lifecycle_status", "review_status"), "stream_network")
  x <- stream_network
  if (nrow(configuration) != 1L || nrow(observation) != 1L || !inherits(x, "sf") || !nrow(x)) {
    .fg_abort("Supply one Configuration, one Observation, and nonempty sf segments.")
  }
  if (!identical(observation$evidence_class, "SOURCE_NETWORK_RETAINED")) {
    .fg_abort("This validator currently supports SOURCE_NETWORK_RETAINED only.")
  }
  oid <- .fg_uuid(observation$stream_network_observation_id, "observation ID")
  cid <- .fg_uuid(configuration$stream_network_configuration_id, "configuration ID")
  ids <- .fg_uuid(x$stream_network_segment_id, "segment IDs")
  if (anyDuplicated(ids)) .fg_abort("Segment IDs must be unique.")
  run_id <- .fg_generate_uuid(1L)
  findings <- list()
  add <- function(code, message, relation = "stream_network_observation", id = oid, related_id = NA_character_) {
    findings[[length(findings) + 1L]] <<- tibble::tibble(
      issue_code = code, affected_relation = relation, affected_object_id = id, message = message,
      related_relation = if (is.na(related_id)) NA_character_ else "stream_network", related_object_id = related_id
    )
  }
  # Reuse constructor metadata rules without changing the supplied Observation.
  args <- as.list(observation[1, intersect(names(observation), names(formals(create_stream_network_observation))), drop = FALSE])
  args$actor <- actor
  args$created_at <- validated_at
  metadata <- tryCatch({ do.call(create_stream_network_observation, args); NULL },
                       error = function(e) conditionMessage(e))
  if (!is.null(metadata)) add("OBSERVATION_METADATA_INVALID", metadata)
  member_ids <- .fg_uuid(configuration_streams$stream_id, "membership stream IDs")
  if (anyDuplicated(member_ids) ||
      any(.fg_uuid(configuration_streams$stream_network_configuration_id, "membership configuration IDs") != cid) ||
      .fg_uuid(observation$stream_network_configuration_id, "observation configuration ID") != cid ||
      !(configuration$configuration_mode %in% c("STREAM", "STUDY_AREA_NETWORK")) ||
      (configuration$configuration_mode == "STREAM" && length(member_ids) != 1L) ||
      (configuration$configuration_mode == "STUDY_AREA_NETWORK" && length(member_ids) < 2L)) {
    add("CONFIGURATION_MEMBERSHIP_INVALID", "Configuration, membership cardinality, and Observation must agree.")
  }
  segment_observations <- .fg_uuid(x$stream_network_observation_id, "segment observation IDs")
  if (any(segment_observations != oid)) {
    add("OBSERVATION_MEMBERSHIP_INVALID", "All segments must belong to the supplied Observation.")
  }
  streams <- .fg_optional_uuid_vector(x$stream_id, "segment stream IDs", nrow(x))
  if (anyNA(streams) || any(!streams %in% member_ids)) {
    add("STREAM_MEMBERSHIP_INVALID", "Every segment needs a Stream in this Configuration.")
  }
  if (isTRUE(observation$coverage_status == "FULL_CONFIGURATION") && !setequal(streams, member_ids)) {
    add("CONFIGURATION_COVERAGE_INVALID", "FULL_CONFIGURATION must represent every configured Stream.")
  }
  reach_ids <- .fg_optional_uuid_vector(x$reach_id, "reach IDs", nrow(x))
  if (any(!is.na(reach_ids))) {
    if (is.null(reaches)) {
      add("REACH_MEMBERSHIP_UNVERIFIED", "Supply governed Reach-Stream mappings for classified Reaches.")
    } else {
      .fg_require_table(reaches, c("reach_id", "stream_id"), "reaches")
      rids <- .fg_uuid(reaches$reach_id, "governed reach IDs")
      rstreams <- .fg_uuid(reaches$stream_id, "governed reach stream IDs")
      selected <- which(!is.na(reach_ids))
      if (anyDuplicated(rids) || anyNA(match(reach_ids[selected], rids)) ||
          !isTRUE(all(rstreams[match(reach_ids[selected], rids)] == streams[selected]))) {
        add("REACH_MEMBERSHIP_INVALID", "Each classified Reach must belong to its segment's Stream.")
      }
    }
  }
  for (i in which(is.na(x$segment_role) | !x$segment_role %in% c("MAINSTEM", "TRIBUTARY", "CONNECTOR", "ARTIFICIAL"))) {
    add("SEGMENT_ROLE_UNRESOLVED", "Supply an explicit, scientifically appropriate segment role.", "stream_network", ids[i])
  }
  if (any(x$direction_status %in% "CONFIRMED" & !x$direction_method %in%
          c("TERRAIN_ELEVATION", "FLOW_ACCUMULATION", "SOURCE_RETAINED", "ANALYST_CONFIRMED"))) {
    add("DIRECTION_METHOD_REQUIRED", "Confirmed directions need an explicit supported evidence method, not LEGACY_UNKNOWN.")
  }
  geometry <- sf::st_geometry(x)
  valid_geometry <- !is.na(sf::st_crs(x)) && !isTRUE(sf::st_is_longlat(x)) &&
    all(sf::st_geometry_type(x) == "LINESTRING") && !any(sf::st_is_empty(x)) &&
    isTRUE(all(sf::st_is_valid(x))) && all(vapply(geometry, inherits, logical(1), "XY")) &&
    isTRUE(all(as.numeric(sf::st_length(x)) > 0))
  if (!valid_geometry) add("GEOMETRY_INVALID", "Segments must be valid, positive-length projected XY LINESTRINGs.")
  tolerance <- observation$topology_tolerance
  unit <- observation$topology_tolerance_unit
  unit_names <- list(METRE = c("metre", "meter", "metres", "meters", "m"),
    FOOT = c("foot", "feet", "international foot", "ft"),
    US_SURVEY_FOOT = c("us survey foot", "us_survey_foot", "us-ft"))
  tolerance_ok <- is.numeric(tolerance) && length(tolerance) == 1L && is.finite(tolerance) && tolerance > 0 &&
    !is.na(unit) && unit %in% names(unit_names) && valid_geometry &&
    isTRUE(tolower(sf::st_crs(x)$units_gdal) %in% unit_names[[unit]])
  if (!tolerance_ok) add("TOPOLOGY_UNITS_INVALID", "A positive topology tolerance must use the actual projected CRS units.")
  if (valid_geometry && !is.na(observation$native_horizontal_crs)) {
    declared <- tryCatch(sf::st_crs(observation$native_horizontal_crs), error = function(e) NULL)
    # File-geodatabase reads may expose the CRS's display Name as $input rather
    # than a reparsable EPSG/WKT string. Accept only the actual CRS's exact Name.
    agrees <- if (is.null(declared)) identical(observation$native_horizontal_crs, sf::st_crs(x)$Name) else
      isTRUE(declared == sf::st_crs(x))
    if (!agrees) add("CRS_METADATA_MISMATCH", "Observation CRS metadata disagrees with segment geometry.")
  }
  if (tolerance_ok) {
    for (f in .fg_stream_geometry_findings(geometry, tolerance)) {
      add(f$code, f$message, "stream_network", ids[f$i], if (is.na(f$j)) NA_character_ else ids[f$j])
    }
  }
  up <- .fg_optional_uuid_vector(x$upstream_node_id, "upstream_node_id", nrow(x))
  down <- .fg_optional_uuid_vector(x$downstream_node_id, "downstream_node_id", nrow(x))
  if (anyNA(up) || anyNA(down)) add("NODE_IDENTITIES_REQUIRED", "Assign all candidate endpoint identities before validation can pass.")
  rebuilt <- NULL
  if (valid_geometry && !anyNA(up) && !anyNA(down) && all(segment_observations == oid)) {
    rebuilt <- tryCatch(connect_stream_network(x), fluvgeo_connectivity_error = function(e) {
      add(e$issue_code, conditionMessage(e)); NULL
    })
  } else if (anyNA(x$direction_status) || any(x$direction_status != "CONFIRMED")) {
    add("DIRECTION_UNRESOLVED", "Every segment requires confirmed direction.")
  }
  if (is.null(nodes)) add("NODE_TABLE_REQUIRED", "Supply the current node table.")
  if (is.null(connections)) add("CONNECTION_TABLE_REQUIRED", "Supply the current connection table.")
  if (!is.null(rebuilt)) {
    expected <- rebuilt$stream_network_node
    if (!is.null(nodes)) {
      .fg_require_table(nodes, names(sf::st_drop_geometry(expected)), "nodes")
      nids <- if (nrow(nodes)) .fg_uuid(nodes$node_id, "node IDs") else character()
      idx <- match(expected$node_id, nids)
      node_ok <- inherits(nodes, "sf") && !anyDuplicated(nids) && setequal(nids, expected$node_id) &&
        isTRUE(sf::st_crs(nodes) == sf::st_crs(x)) &&
        identical(sf::st_as_binary(sf::st_geometry(nodes)[idx]), sf::st_as_binary(sf::st_geometry(expected))) &&
        isTRUE(all(as.matrix(sf::st_drop_geometry(nodes)[idx, c("stream_network_observation_id", "in_degree", "out_degree", "node_topology")]) ==
                     as.matrix(sf::st_drop_geometry(expected)[, c("stream_network_observation_id", "in_degree", "out_degree", "node_topology")])) )
      if (!node_ok) add("NODE_TABLE_MISMATCH", "Node identities, locations, counts, or labels disagree with current segment endpoints.")
    }
    if (!is.null(connections)) {
      expected_c <- rebuilt$stream_network_connection
      .fg_require_table(connections, names(expected_c), "connections")
      row_key <- function(d) do.call(paste, c(as.list(d[names(expected_c)]), sep = "\r"))
      if (anyDuplicated(row_key(connections)) || !setequal(row_key(connections), row_key(expected_c))) {
        add("CONNECTION_TABLE_MISMATCH", "Stored connections disagree with recomputed hydroloom relationships, including outlets/diversions.")
      }
    }
  }
  if (is.null(sources)) {
    if (level == "ACCEPTANCE") add("SOURCE_LINEAGE_REQUIRED", "Retained-network acceptance requires source lineage for every segment.")
  } else {
    .fg_require_table(sources, c("stream_network_source_id", "stream_network_segment_id"), "sources")
    sids <- if (nrow(sources)) .fg_uuid(sources$stream_network_source_id, "source IDs") else character()
    if (anyDuplicated(sids) || !setequal(sources$stream_network_segment_id, ids)) {
      add("SOURCE_LINEAGE_INVALID", "Source IDs must be unique, non-orphaned, and cover every current segment.")
    }
  }
  if (is.null(operations)) {
    if (level == "ACCEPTANCE") add("OPERATION_TABLE_REQUIRED", "Supply the applied operation table, even if empty.")
  } else {
    .fg_require_table(operations, c("stream_network_operation_id", "stream_network_segment_id", "stream_network_source_id",
      "operation_sequence", "operation_code", "operation_notes", "performed_at", "performed_by"), "operations")
    opids <- if (nrow(operations)) .fg_uuid(operations$stream_network_operation_id, "operation IDs") else character()
    seq <- operations$operation_sequence
    if (anyDuplicated(opids) || any(!operations$stream_network_segment_id %in% ids) || !is.numeric(seq) ||
        anyNA(seq) || any(!is.finite(seq) | seq < 1 | seq %% 1 != 0) ||
        anyDuplicated(operations[c("stream_network_segment_id", "operation_sequence")]) ||
        any(!operations$operation_code %in% c("CONSOLIDATE_SEGMENTS", "REVERSE_DIRECTION",
          "CONFIRM_DIRECTION", "ASSIGN_NETWORK_NODES", "CLASSIFY_SEGMENT_ROLE")) ||
        anyNA(operations$operation_notes) || any(!nzchar(trimws(operations$operation_notes))) ||
        !inherits(operations$performed_at, "POSIXt") || anyNA(operations$performed_at) ||
        anyNA(operations$performed_by) || any(!nzchar(trimws(operations$performed_by)))) {
      add("OPERATION_HISTORY_INVALID", "Operations need unique IDs/order, supported codes, current segment FKs, actor, time, and notes.")
    }
    linked <- which(!is.na(operations$stream_network_source_id))
    if (length(linked) && (is.null(sources) ||
        !isTRUE(all(sources$stream_network_segment_id[match(operations$stream_network_source_id[linked], sources$stream_network_source_id)] ==
                    operations$stream_network_segment_id[linked])))) {
      add("OPERATION_SOURCE_MISMATCH", "Each operation source FK must belong to that operation's segment.")
    }
    classified <- which(operations$operation_code %in% "CLASSIFY_SEGMENT_ROLE")
    if (length(classified)) {
      role_ok <- "segment_role" %in% names(operations) &&
        isTRUE(all(operations$segment_role[classified] %in% c("MAINSTEM", "TRIBUTARY", "CONNECTOR", "ARTIFICIAL")))
      if (role_ok && is.numeric(seq) && !anyNA(seq)) {
        latest <- classified[order(seq[classified], decreasing = TRUE)]
        latest <- latest[!duplicated(operations$stream_network_segment_id[latest])]
        role_ok <- isTRUE(all(operations$segment_role[latest] == x$segment_role[match(operations$stream_network_segment_id[latest], ids)]))
      }
      if (!role_ok) add("ROLE_HISTORY_MISMATCH", "The latest explicit role operation must agree with the current segment classification.")
    }
  }
  if (level == "ACCEPTANCE") {
    if (!isTRUE(observation$lifecycle_status == "ACTIVE") ||
        !observation$review_status %in% c("DRAFT", "READY_FOR_REVIEW", "ACCEPTED") ||
        !isTRUE(all(x$lifecycle_status == "ACTIVE")) ||
        any(!x$review_status %in% c("PENDING", "ACCEPTED"))) {
      add("LIFECYCLE_NOT_ELIGIBLE", "Rejected or retired candidates/observations require an explicit reopen before acceptance.")
    }
    if (is.null(review_features)) {
      add("REQUIRED_REVIEW_PENDING", "Supply explicit inspection decisions for the current candidates.")
    } else {
      .fg_require_table(review_features, c("stream_network_review_id", "stream_network_segment_id", "stream_network_observation_id", "operation_code",
        "decision", "decision_at", "decision_by", "decision_notes"), "review_features")
      review_ids <- if (nrow(review_features)) .fg_uuid(review_features$stream_network_review_id, "review IDs") else character()
      if (anyDuplicated(review_ids) || !setequal(review_features$stream_network_segment_id, ids) ||
          anyNA(review_features$decision) || any(review_features$decision != "ACCEPT") ||
          !isTRUE(all(review_features$stream_network_observation_id == oid)) ||
          !isTRUE(all(review_features$operation_code == "INSPECT"))) {
        add("REQUIRED_REVIEW_PENDING", "Every current segment needs accepted INSPECT decisions; pending/rejected or foreign reviews block acceptance.")
      }
      reviewed <- review_features$decision %in% "ACCEPT"
      if (!inherits(review_features$decision_at, "POSIXt") || anyNA(review_features$decision_at[reviewed]) ||
          anyNA(review_features$decision_by[reviewed]) || any(!nzchar(trimws(review_features$decision_by[reviewed]))) ||
          anyNA(review_features$decision_notes[reviewed]) || any(!nzchar(trimws(review_features$decision_notes[reviewed])))) {
        add("REVIEW_PROVENANCE_REQUIRED", "Accepted inspections require explicit reviewer, time, and notes.")
      }
      if (!inherits(review_features, "sf") || (valid_geometry &&
          (!isTRUE(sf::st_crs(review_features) == sf::st_crs(x)) ||
           !identical(sf::st_as_binary(sf::st_geometry(review_features)),
                      sf::st_as_binary(geometry[match(review_features$stream_network_segment_id, ids)]))))) {
        add("REVIEW_GEOMETRY_STALE", "Inspection geometry no longer matches the current candidate geometry.")
      }
      if (!inherits(x$modified_at, "POSIXt") || anyNA(x$modified_at) ||
          (inherits(review_features$decision_at, "POSIXt") &&
           any(review_features$decision_at[reviewed] < x$modified_at[match(review_features$stream_network_segment_id[reviewed], ids)], na.rm = TRUE))) {
        add("REVIEW_STALE", "Accepted inspections must postdate the current candidate modifications, including role changes.")
      }
    }
    qualified <- !isTRUE(observation$coverage_status == "FULL_CONFIGURATION") || !isTRUE(observation$provenance_completeness == "COMPLETE")
    if (!is.null(rebuilt)) qualified <- qualified || sum(is.na(rebuilt$stream_network_connection$downstream_segment_id)) > 1L
    if (qualified && (is.na(observation$review_notes) || !nzchar(trimws(observation$review_notes)))) {
      add("OBSERVATION_QUALIFICATION_REQUIRED", "Review notes must explicitly qualify incomplete coverage/provenance or multiple observed outlets.")
    }
  }
  details <- if (length(findings)) dplyr::bind_rows(findings) else tibble::tibble(
    issue_code = character(), affected_relation = character(), affected_object_id = character(), message = character(),
    related_relation = character(), related_object_id = character())
  count <- nrow(details)
  issues <- tibble::tibble(
    stream_network_validation_issue_id = .fg_generate_uuid(count),
    stream_network_validation_run_id = rep(run_id, count), issue_code = details$issue_code,
    severity = rep("ERROR", count), affected_relation = details$affected_relation,
    affected_object_id = details$affected_object_id, related_relation = details$related_relation,
    related_object_id = details$related_object_id, message = details$message,
    analyst_disposition = rep("UNRESOLVED", count), disposition_at = rep(as.POSIXct(NA, tz = "UTC"), count),
    disposition_by = rep(NA_character_, count), disposition_notes = rep(NA_character_, count)
  )
  run <- tibble::tibble(stream_network_validation_run_id = run_id, stream_network_observation_id = oid,
    validation_level = level, result = if (count) "REVIEW_REQUIRED" else "PASS",
    model_version = "FGDB_STREAM_NETWORK_1", validator_version = "RETAINED_VALIDATION_0.1",
    validated_at = validated_at, validated_by = actor)
  list(stream_network_validation_run = run, stream_network_validation_issue = issues)
}
