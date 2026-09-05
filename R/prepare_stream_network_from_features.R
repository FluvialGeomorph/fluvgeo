#' Prepare retained Stream Network features for analyst review
#'
#' Normalizes retained linework and produces spatial inspection features for
#' direction and topology findings. Source coordinate order is not evidence of
#' downstream direction. An optional source DEM automatically establishes
#' endpoint-based direction and reverses supported candidates on a copy.
#' Optional consolidation first joins raw continuations into logical links.
#'
#' Checks cover duplicate geometry (including reversed lines), self-intersections,
#' closed segments, intersections involving a line interior, and noncoincident
#' endpoints within the Observation's topology tolerance. Exact shared endpoints
#' are permitted. Intersection findings require interpretation: a crossing is
#' not automatically a hydrologic connection.
#'
#' This retained-source slice does not detect every network defect: disconnected
#' components, multi-segment cycles, near endpoint-to-interior gaps, and missing
#' Stream/Reach boundary splits remain outside its checks. It does not assign
#' node UUIDs. Its WORKING result is always REVIEW_REQUIRED because node
#' identities and segment roles remain unresolved after direction correction.
#'
#' @inheritParams normalize_retained_stream_network
#' @param review_mode CREATE_REVIEW_FEATURES returns pending INSPECT features;
#'   VALIDATE_ONLY assesses but does not apply DEM direction corrections and
#'   returns an empty, typed review layer.
#' @param dem Optional single-band terra SpatRaster representing the source
#'   Stream DEM, in the linework CRS. NULL skips DEM direction assessment.
#'   Automatic correction requires finite DEM values at every endpoint. An
#'   incomplete DEM fails before any corrections are returned; VALIDATE_ONLY
#'   reports per-endpoint coverage diagnostics to help select the correct input.
#' @param consolidate Logical; FALSE retains raw segmentation (the compatible
#'   default). TRUE builds logical links before DEM orientation, preserving
#'   Stream/Reach boundaries and source lineage. Raw endpoint DEM coverage is
#'   checked before merging. VALIDATE_ONLY never consolidates or records edits.
#' @param protected_nodes Optional exact endpoint POINTs passed to
#'   \code{\link{build_logical_stream_links}} when consolidating. Boundaries inside source
#'   lines must first be split explicitly; they are not inferred or snapped.
#'
#' @return A named list with stream_network, stream_network_source,
#'   stream_network_review, stream_network_validation_run, and
#'   stream_network_validation_issue, stream_network_operation, and
#'   stream_network_direction_evidence. Direction evidence is empty without
#'   a DEM; operations also record applied logical-link consolidation.
#'   Evidence records pre-orientation logical-link endpoint values, raster reference, method,
#'   action, and an operation link for applied corrections/classifications.
#'   Review Shape is the affected candidate
#'   line, not a proposed repair. INSPECT decisions cannot authorize geometry
#'   changes; concrete repair proposals and their application are later steps.
#' @export
prepare_stream_network_from_features <- function(
    stream_network,
    source_mappings,
    configuration,
    configuration_streams,
    observation,
    actor,
    review_mode = c("CREATE_REVIEW_FEATURES", "VALIDATE_ONLY"),
    dem = NULL,
    consolidate = FALSE,
    protected_nodes = NULL) {
  review_mode <- match.arg(review_mode)
  if (!is.logical(consolidate) || length(consolidate) != 1L || is.na(consolidate)) {
    .fg_abort("`consolidate` must be TRUE or FALSE.")
  }
  if (!consolidate && !is.null(protected_nodes)) {
    .fg_abort("`protected_nodes` requires consolidate = TRUE.")
  }
  result <- normalize_retained_stream_network(
    stream_network, source_mappings, configuration, configuration_streams,
    observation, actor
  )
  tolerance <- .fg_positive_optional(
    observation$topology_tolerance, "observation$topology_tolerance",
    required = TRUE
  )
  tolerance_unit <- .fg_required_text(
    observation$topology_tolerance_unit, "observation$topology_tolerance_unit"
  )
  # Compare declared units with the actual geometry CRS, not just metadata.
  crs_unit <- sf::st_crs(stream_network)$units_gdal
  supported_units <- list(
    METRE = c("metre", "meter", "metres", "meters", "m"),
    FOOT = c("foot", "feet", "international foot", "ft"),
    US_SURVEY_FOOT = c("us survey foot", "us_survey_foot", "us-ft")
  )
  if (is.null(crs_unit) || is.na(crs_unit) ||
      !tolower(crs_unit) %in% supported_units[[tolerance_unit]]) {
    .fg_abort(paste(
      "`observation$topology_tolerance_unit` must match the projected CRS:",
      "METRE, FOOT, or US_SURVEY_FOOT."
    ))
  }

  segments <- result$stream_network
  require_coverage <- function(direction) {
    outside <- direction$reason_code == "ENDPOINT_OUTSIDE_DEM"
    nodata <- direction$reason_code == "ENDPOINT_DEM_NODATA"
    if (any(outside | nodata)) {
      .fg_abort(sprintf(paste(
        "Source DEM endpoint coverage is incomplete: %d segments outside its extent;",
        "%d additional segments with NoData/nonfinite endpoint values.",
        "Supply the matching full source DEM, or use review_mode = 'VALIDATE_ONLY'",
        "to inspect coverage. No direction corrections were returned."
      ), sum(outside), sum(nodata)))
    }
  }
  merged <- rep(FALSE, nrow(segments))
  if (consolidate && review_mode != "VALIDATE_ONLY") {
    if (!is.null(dem)) {
      # Do not make absent source elevations disappear by removing raw endpoints.
      require_coverage(orient_lines_from_dem(segments, dem)$direction)
    }
    logical <- build_logical_stream_links(
      segments, boundary_fields = c("stream_id", "reach_id"),
      protected_nodes = protected_nodes, tolerance = tolerance
    )
    membership <- logical$membership
    members <- split(membership$input_row, membership$link_row)
    # split() orders numeric groups numerically; keep the explicit output order.
    members <- members[as.character(seq_len(nrow(logical$links)))]
    first <- vapply(members, `[`, integer(1), 1L)
    merged <- lengths(members) > 1L
    old_ids <- segments$stream_network_segment_id
    segments <- segments[first, ]
    sf::st_geometry(segments) <- sf::st_geometry(logical$links)
    segments$stream_network_segment_id[merged] <- .fg_generate_uuid(sum(merged))
    segments$source_feature_key[merged] <- NA_character_
    source_link <- membership$link_row[
      match(match(result$stream_network_source$stream_network_segment_id, old_ids),
            membership$input_row)
    ]
    result$stream_network_source$stream_network_segment_id <-
      segments$stream_network_segment_id[source_link]
    result$stream_network_source$geometry_modified <-
      result$stream_network_source$geometry_modified | merged[source_link]
  }
  n <- nrow(segments)
  segment_ids <- segments$stream_network_segment_id
  # A segment can now have several sources. Never falsely select one of them
  # for a whole-link operation or review; the segment FK retrieves all sources.
  source_ids <- vapply(segment_ids, function(id) {
    ids <- result$stream_network_source$stream_network_source_id[
      result$stream_network_source$stream_network_segment_id == id]
    if (length(ids) == 1L) ids else NA_character_
  }, character(1), USE.NAMES = FALSE)
  run <- result$stream_network_validation_run
  run$validator_version <- "RETAINED_ASSESSMENT_0.3"
  direction <- tibble::tibble(
    input_row = integer(), start_elevation = double(), end_elevation = double(),
    start_sample_status = character(), end_sample_status = character(),
    action = character(), reason_code = character(), method = character(),
    dem_band = character(), dem_source = character()
  )
  applied <- integer()
  if (!is.null(dem)) {
    oriented <- orient_lines_from_dem(segments, dem)
    direction <- oriented$direction
    if (review_mode != "VALIDATE_ONLY") {
      require_coverage(direction)
      applied <- which(direction$action != "UNRESOLVED")
      segments <- oriented$lines
      segments$direction_status[applied] <- "CONFIRMED"
      segments$direction_method[applied] <- "TERRAIN_ELEVATION"
      segments$modified_at[applied] <- run$validated_at
      segments$modified_by[applied] <- actor
      reversed <- which(direction$action == "REVERSE")
      result$stream_network_source$geometry_modified[
        result$stream_network_source$stream_network_segment_id %in% segment_ids[reversed]
      ] <- TRUE
    }
  }
  geometry <- sf::st_geometry(segments)
  operation_ids <- .fg_generate_uuid(length(applied))
  operations <- tibble::tibble(
    stream_network_operation_id = operation_ids,
    stream_network_segment_id = segment_ids[applied],
    stream_network_source_id = source_ids[applied],
    operation_sequence = 1L + as.integer(merged[applied]),
    operation_code = ifelse(direction$action[applied] == "REVERSE",
                            "REVERSE_DIRECTION", "CONFIRM_DIRECTION"),
    tolerance_value = rep(NA_real_, length(applied)),
    tolerance_unit = rep(NA_character_, length(applied)),
    target_node_id = rep(NA_character_, length(applied)),
    stream_id = rep(NA_character_, length(applied)),
    reach_id = rep(NA_character_, length(applied)),
    operation_notes = rep("Automatic direction assignment using DEM_ENDPOINTS_1; see direction evidence.",
                          length(applied)),
    performed_at = rep(run$validated_at, length(applied)),
    performed_by = rep(actor, length(applied))
  )
  # ifelse(logical(0), ...) returns logical(0); retain the table's text schema.
  operations$operation_code <- as.character(operations$operation_code)
  merge_operations <- tibble::tibble(
    stream_network_operation_id = .fg_generate_uuid(sum(merged)),
    stream_network_segment_id = segment_ids[merged],
    stream_network_source_id = rep(NA_character_, sum(merged)),
    operation_sequence = rep(1L, sum(merged)),
    operation_code = rep("CONSOLIDATE_SEGMENTS", sum(merged)),
    tolerance_value = rep(tolerance, sum(merged)),
    tolerance_unit = rep(tolerance_unit, sum(merged)),
    target_node_id = rep(NA_character_, sum(merged)),
    stream_id = rep(NA_character_, sum(merged)),
    reach_id = rep(NA_character_, sum(merged)),
    operation_notes = rep(paste(
      "LOGICAL_LINKS_1: exact-endpoint degree-two concatenation;",
      "Stream/Reach and protected boundaries retained; tolerance protects near misses, not snapping;",
      "sfnetworks", utils::packageVersion("sfnetworks"),
      "tidygraph", utils::packageVersion("tidygraph"),
      "igraph", utils::packageVersion("igraph")
    ), sum(merged)),
    performed_at = rep(run$validated_at, sum(merged)),
    performed_by = rep(actor, sum(merged))
  )
  operations <- dplyr::bind_rows(merge_operations, operations)
  evidence_operation_ids <- rep(NA_character_, nrow(direction))
  evidence_operation_ids[applied] <- operation_ids
  elevation_unit <- if ("vertical_unit" %in% names(observation)) {
    .fg_optional_text(observation$vertical_unit, "observation$vertical_unit")
  } else NA_character_
  evidence <- tibble::tibble(
    stream_network_segment_id = segment_ids[direction$input_row],
    stream_network_operation_id = evidence_operation_ids,
    start_elevation = direction$start_elevation,
    end_elevation = direction$end_elevation,
    start_sample_status = direction$start_sample_status,
    end_sample_status = direction$end_sample_status,
    elevation_unit = rep(elevation_unit, nrow(direction)),
    action = direction$action,
    reason_code = direction$reason_code,
    method = direction$method,
    dem_band = direction$dem_band,
    dem_source = direction$dem_source
  )

  findings <- list()
  add_finding <- function(i, code, message, j = NA_integer_) {
    findings[[length(findings) + 1L]] <<- list(
      i = i, j = j, code = code, message = message
    )
  }
  for (i in seq_len(n)) {
    if (i %in% applied) {
      add_finding(i, "SEGMENT_REVIEW_REQUIRED",
                  "Direction resolved by DEM; node identities and segment role remain unresolved.")
    } else {
      detail <- if (is.null(dem)) "No DEM supplied." else paste(
        "DEM endpoint assessment:", direction$reason_code[i],
        "Action:", direction$action[i],
        if (review_mode == "VALIDATE_ONLY") "Corrections not applied in VALIDATE_ONLY mode." else ""
      )
      code <- if (!is.null(dem) && direction$reason_code[i] %in%
                  c("ENDPOINT_OUTSIDE_DEM", "ENDPOINT_DEM_NODATA")) {
        "DEM_COVERAGE_INCOMPLETE"
      } else "DIRECTION_UNRESOLVED"
      add_finding(i, code, paste(
        detail, "Direction, node identities, and segment role remain unresolved."
      ))
    }
  }
  simple <- sf::st_is_simple(geometry)
  endpoints <- sf::st_sfc(lapply(geometry, function(line) {
    sf::st_multipoint(unclass(line)[c(1L, nrow(line)), 1:2, drop = FALSE])
  }), crs = sf::st_crs(geometry))
  for (i in seq_len(n)) {
    if (!simple[i]) {
      add_finding(i, "SELF_INTERSECTION", "Inspect this self-intersecting segment.")
    }
    if (all(endpoints[[i]][1L, ] == endpoints[[i]][2L, ])) {
      add_finding(i, "CLOSED_SEGMENT", "Inspect this segment whose endpoints coincide.")
    }
  }

  # Sparse candidate pairs avoid constructing a dense all-pairs distance matrix.
  nearby <- sf::st_is_within_distance(geometry, dist = tolerance)
  for (i in seq_len(n)) {
    for (j in nearby[[i]][nearby[[i]] > i]) {
      if (length(sf::st_equals(geometry[i], geometry[j])[[1L]])) {
        add_finding(i, "DUPLICATE_GEOMETRY", paste(
          "These segments cover the same geometry; review their source lineage",
          "before deciding which geometry to retain."
        ), j)
        next
      }
      relation <- sf::st_relate(geometry[i], geometry[j])[1L, 1L]
      if (any(substring(relation, c(1L, 2L, 4L), c(1L, 2L, 4L)) != "F")) {
        add_finding(i, "INTERIOR_INTERSECTION", paste(
          "The lines intersect at an interior or overlap. Determine whether",
          "a confluence split, overlap repair, or qualified crossing is needed."
        ), j)
      }
      # Measure all four endpoint pairs, including cases where a different pair
      # coincides exactly. Never snap endpoints automatically.
      a <- sf::st_cast(endpoints[i], "POINT")
      b <- sf::st_cast(endpoints[j], "POINT")
      distances <- as.numeric(sf::st_distance(a, b))
      if (any(distances > 0 & distances <= tolerance)) {
        add_finding(i, "ENDPOINT_NEAR_MISS", paste(
          "Noncoincident endpoints are within the observation tolerance.",
          "Inspect both segments before choosing a snap target or retaining a gap."
        ), j)
      }
    }
  }

  affected <- vapply(findings, `[[`, integer(1), "i")
  related <- vapply(findings, `[[`, integer(1), "j")
  codes <- vapply(findings, `[[`, character(1), "code")
  messages <- vapply(findings, `[[`, character(1), "message")
  count <- length(findings)
  issues <- tibble::tibble(
    stream_network_validation_issue_id = .fg_generate_uuid(count),
    stream_network_validation_run_id = rep(run$stream_network_validation_run_id, count),
    issue_code = codes,
    severity = rep("ERROR", count),
    affected_relation = rep("stream_network", count),
    affected_object_id = segment_ids[affected],
    related_relation = ifelse(is.na(related),
                             ifelse(is.na(source_ids[affected]), NA_character_, "stream_network_source"),
                             "stream_network"),
    related_object_id = ifelse(is.na(related), source_ids[affected], segment_ids[related]),
    message = messages,
    analyst_disposition = rep("UNRESOLVED", count),
    disposition_at = rep(as.POSIXct(NA, tz = "UTC"), count),
    disposition_by = rep(NA_character_, count),
    disposition_notes = rep(NA_character_, count)
  )
  review <- sf::st_sf(
    tibble::tibble(
      stream_network_review_id = .fg_generate_uuid(count),
      stream_network_observation_id = rep(run$stream_network_observation_id, count),
      stream_network_segment_id = segment_ids[affected],
      stream_network_source_id = source_ids[affected],
      stream_network_validation_issue_id = issues$stream_network_validation_issue_id,
      operation_code = rep("INSPECT", count),
      reason_code = codes,
      proposed_tolerance_value = rep(NA_real_, count),
      proposed_tolerance_unit = rep(NA_character_, count),
      proposed_node_id = rep(NA_character_, count),
      proposed_stream_id = rep(NA_character_, count),
      proposed_reach_id = rep(NA_character_, count),
      decision = rep("PENDING", count),
      decision_at = rep(as.POSIXct(NA, tz = "UTC"), count),
      decision_by = rep(NA_character_, count),
      decision_notes = rep(NA_character_, count)
    ),
    Shape = geometry[affected]
  )
  if (review_mode == "VALIDATE_ONLY") {
    review <- review[0, ]
  }
  list(
    stream_network = segments,
    stream_network_source = result$stream_network_source,
    stream_network_review = review,
    stream_network_validation_run = run,
    stream_network_validation_issue = issues,
    stream_network_operation = operations,
    stream_network_direction_evidence = evidence
  )
}
