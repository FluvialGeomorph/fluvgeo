#' Prepare retained Stream Network features for analyst review
#'
#' Normalizes retained linework and produces spatial inspection features for
#' direction and topology findings. Source coordinate order is not evidence of
#' downstream direction. No geometry repairs or analyst decisions are applied.
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
#' node UUIDs or confirm direction. Its WORKING result is always REVIEW_REQUIRED.
#'
#' @inheritParams normalize_retained_stream_network
#' @param review_mode CREATE_REVIEW_FEATURES returns pending INSPECT features;
#'   VALIDATE_ONLY returns the same findings and an empty, typed review layer.
#'
#' @return A named list with stream_network, stream_network_source,
#'   stream_network_review, stream_network_validation_run, and
#'   stream_network_validation_issue. Review Shape is the affected candidate
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
    review_mode = c("CREATE_REVIEW_FEATURES", "VALIDATE_ONLY")) {
  review_mode <- match.arg(review_mode)
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
  geometry <- sf::st_geometry(segments)
  n <- nrow(segments)
  segment_ids <- segments$stream_network_segment_id
  source_ids <- result$stream_network_source$stream_network_source_id
  run <- result$stream_network_validation_run
  run$validator_version <- "RETAINED_ASSESSMENT_0.1"

  findings <- list()
  add_finding <- function(i, code, message, j = NA_integer_) {
    findings[[length(findings) + 1L]] <<- list(
      i = i, j = j, code = code, message = message
    )
  }
  for (i in seq_len(n)) {
    add_finding(i, "DIRECTION_UNRESOLVED", paste(
      "Confirm downstream-to-upstream coordinate order using analyst or terrain",
      "evidence. Node identities and segment role also remain unresolved."
    ))
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
    related_relation = ifelse(is.na(related), "stream_network_source", "stream_network"),
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
    stream_network_validation_issue = issues
  )
}
