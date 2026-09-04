# Stream Network relational constructors ------------------------------------

.fg_uuid_pattern <- paste0(
  "^[{]?",
  "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[1-5][0-9A-Fa-f]{3}-",
  "[89ABab][0-9A-Fa-f]{3}-[0-9A-Fa-f]{12}",
  "[}]?$"
)

.fg_abort <- function(message) {
  stop(message, call. = FALSE)
}

.fg_required_text <- function(x, name, length = 1L) {
  if (!is.character(x) || base::length(x) != length || anyNA(x) ||
      any(!nzchar(trimws(x)))) {
    .fg_abort(sprintf("`%s` must contain %s nonempty character value%s.",
                      name, length, if (length == 1L) "" else "s"))
  }
  trimws(x)
}

.fg_optional_text <- function(x, name) {
  if (base::length(x) != 1L || (!is.na(x) && !is.character(x))) {
    .fg_abort(sprintf("`%s` must be one character value or `NA`.", name))
  }
  if (!is.na(x)) {
    x <- trimws(x)
    if (!nzchar(x)) {
      .fg_abort(sprintf("`%s` cannot be blank.", name))
    }
  }
  x
}

.fg_uuid <- function(x, name) {
  x <- .fg_required_text(x, name, length = base::length(x))
  if (!length(x) || any(!grepl(.fg_uuid_pattern, x))) {
    .fg_abort(sprintf("`%s` must contain canonical UUID values.", name))
  }
  tolower(gsub("[{}]", "", x))
}

.fg_optional_uuid <- function(x, name) {
  if (base::length(x) != 1L) {
    .fg_abort(sprintf("`%s` must be one UUID value or `NA`.", name))
  }
  if (is.na(x)) {
    return(NA_character_)
  }
  .fg_uuid(x, name)
}

.fg_timestamp <- function(x, name) {
  if (!inherits(x, "POSIXt") || base::length(x) != 1L || is.na(x)) {
    .fg_abort(sprintf("`%s` must be one nonmissing date-time value.", name))
  }
  as.POSIXct(x, tz = "UTC")
}

.fg_choice <- function(x, choices, name) {
  x <- .fg_required_text(x, name)
  if (!x %in% choices) {
    .fg_abort(sprintf("`%s` must be one of: %s.",
                      name, paste(choices, collapse = ", ")))
  }
  x
}

.fg_positive_optional <- function(x, name, required = FALSE) {
  if (base::length(x) != 1L || (!is.na(x) && (!is.numeric(x) || !is.finite(x)))) {
    .fg_abort(sprintf("`%s` must be one finite numeric value or `NA`.", name))
  }
  if (required && is.na(x)) {
    .fg_abort(sprintf("`%s` is required.", name))
  }
  if (!is.na(x) && x <= 0) {
    .fg_abort(sprintf("`%s` must be positive.", name))
  }
  as.numeric(x)
}

.fg_generate_uuid <- function(n) {
  if (length(n) != 1L || is.na(n) || n < 0L || n != as.integer(n)) {
    .fg_abort("`n` must be one nonnegative integer.")
  }
  n <- as.integer(n)
  if (!n) {
    return(character())
  }

  vapply(seq_len(n), function(i) {
    bytes <- as.integer(openssl::rand_bytes(16L))
    bytes[7L] <- bitwOr(bitwAnd(bytes[7L], 15L), 64L)
    bytes[9L] <- bitwOr(bitwAnd(bytes[9L], 63L), 128L)
    hex <- sprintf("%02x", bytes)
    paste0(
      paste0(hex[1L:4L], collapse = ""), "-",
      paste0(hex[5L:6L], collapse = ""), "-",
      paste0(hex[7L:8L], collapse = ""), "-",
      paste0(hex[9L:10L], collapse = ""), "-",
      paste0(hex[11L:16L], collapse = "")
    )
  }, character(1))
}

.fg_optional_uuid_vector <- function(x, name, length) {
  if (base::length(x) != length ||
      (!is.character(x) && !all(is.na(x)))) {
    .fg_abort(sprintf(
      "`%s` must contain %s UUID value%s or `NA`.",
      name,
      length,
      if (length == 1L) "" else "s"
    ))
  }
  x <- as.character(x)
  blank <- !is.na(x) & !nzchar(trimws(x))
  if (any(blank)) {
    .fg_abort(sprintf("`%s` cannot contain blank values.", name))
  }
  present <- !is.na(x)
  x[present] <- .fg_uuid(x[present], name)
  x
}

.fg_source_text <- function(x, field) {
  index <- match(tolower(field), tolower(names(x)))
  if (is.na(index)) {
    return(rep(NA_character_, nrow(x)))
  }
  value <- as.character(x[[index]])
  value[is.na(value)] <- NA_character_
  value
}

#' Create Stream Network Configuration relations
#'
#' Creates the normalized configuration and Stream-membership tables used by
#' the local Stream Geodatabase and FGDB. The function does not query or infer
#' enterprise identities.
#'
#' @param stream_network_configuration_id Immutable configuration UUID.
#' @param study_area_id Immutable Study Area UUID.
#' @param configuration_name Human-readable name unique within the Study Area.
#' @param configuration_mode Either `"STREAM"` or `"STUDY_AREA_NETWORK"`.
#' @param streams Data frame containing governed `stream_id` and reviewable
#'   `stream_name` columns.
#' @param description Optional scientific-purpose description.
#' @param actor Stable actor or process identifier.
#' @param created_at Creation date-time; converted to UTC.
#'
#' @return A named list containing `stream_network_configuration` and
#'   `stream_network_configuration_stream` tibbles.
#' @export
create_stream_network_configuration <- function(
    stream_network_configuration_id,
    study_area_id,
    configuration_name,
    configuration_mode = c("STREAM", "STUDY_AREA_NETWORK"),
    streams,
    description = NA_character_,
    actor,
    created_at = Sys.time()) {
  configuration_mode <- match.arg(configuration_mode)
  configuration_id <- .fg_uuid(
    stream_network_configuration_id,
    "stream_network_configuration_id"
  )
  study_area_id <- .fg_uuid(study_area_id, "study_area_id")
  configuration_name <- .fg_required_text(
    configuration_name,
    "configuration_name"
  )
  description <- .fg_optional_text(description, "description")
  actor <- .fg_required_text(actor, "actor")
  created_at <- .fg_timestamp(created_at, "created_at")

  if (!is.data.frame(streams) ||
      !all(c("stream_id", "stream_name") %in% names(streams))) {
    .fg_abort("`streams` must be a data frame with `stream_id` and `stream_name` columns.")
  }
  if (!nrow(streams)) {
    .fg_abort("`streams` must contain at least one governed Stream.")
  }

  stream_ids <- .fg_uuid(streams$stream_id, "streams$stream_id")
  stream_names <- .fg_required_text(
    streams$stream_name,
    "streams$stream_name",
    length = nrow(streams)
  )
  if (anyDuplicated(stream_ids)) {
    .fg_abort("`streams$stream_id` must be unique within the configuration.")
  }
  if (configuration_mode == "STREAM" && nrow(streams) != 1L) {
    .fg_abort("A `STREAM` configuration must contain exactly one Stream.")
  }
  if (configuration_mode == "STUDY_AREA_NETWORK" && nrow(streams) < 2L) {
    .fg_abort("A `STUDY_AREA_NETWORK` configuration must contain at least two Streams.")
  }

  configuration <- tibble::tibble(
    stream_network_configuration_id = configuration_id,
    study_area_id = study_area_id,
    configuration_name = configuration_name,
    configuration_mode = configuration_mode,
    description = description,
    created_at = created_at,
    created_by = actor,
    modified_at = created_at,
    modified_by = actor,
    lifecycle_status = "ACTIVE"
  )

  membership <- tibble::tibble(
    stream_network_configuration_id = rep(configuration_id, nrow(streams)),
    stream_id = stream_ids,
    stream_name = stream_names,
    membership_role = "SUBJECT",
    created_at = rep(created_at, nrow(streams)),
    created_by = rep(actor, nrow(streams))
  )

  list(
    stream_network_configuration = configuration,
    stream_network_configuration_stream = membership
  )
}

#' Create a Stream Network Observation relation
#'
#' Creates one time-specific Stream Network Observation row. Conditional
#' requirements distinguish current producer output from qualified legacy
#' evidence without inventing missing provenance.
#'
#' @param stream_network_observation_id Immutable observation UUID.
#' @param stream_network_configuration_id Governing configuration UUID.
#' @param observation_year Four-digit evidence year.
#' @param observation_month Optional month.
#' @param observation_day Optional day; requires a month.
#' @param evidence_class Controlled evidence class.
#' @param coverage_status Controlled coverage status.
#' @param source_terrain_id Optional governed terrain UUID.
#' @param source_terrain_label Optional review label.
#' @param source_terrain_fingerprint Optional 64-character hexadecimal hash.
#' @param derivation_method_id Stable scientific method identifier.
#' @param method_version Method version; required for complete provenance.
#' @param threshold_value Optional positive initiation threshold.
#' @param threshold_unit Unit paired with `threshold_value`.
#' @param topology_tolerance Positive topology tolerance.
#' @param topology_tolerance_unit Horizontal unit for the tolerance.
#' @param native_horizontal_crs Source horizontal CRS identifier.
#' @param native_vertical_datum Optional vertical datum identifier.
#' @param horizontal_unit Source horizontal unit.
#' @param vertical_unit Unit paired with `native_vertical_datum`.
#' @param cell_size Optional positive terrain cell size; required for direct
#'   terrain derivation.
#' @param provenance_completeness Controlled provenance-completeness code.
#' @param actor Stable actor or process identifier.
#' @param created_at Creation date-time; converted to UTC.
#'
#' @return A one-row `stream_network_observation` tibble.
#' @export
create_stream_network_observation <- function(
    stream_network_observation_id,
    stream_network_configuration_id,
    observation_year,
    observation_month = NA_integer_,
    observation_day = NA_integer_,
    evidence_class,
    coverage_status,
    source_terrain_id = NA_character_,
    source_terrain_label = NA_character_,
    source_terrain_fingerprint = NA_character_,
    derivation_method_id,
    method_version = NA_character_,
    threshold_value = NA_real_,
    threshold_unit = NA_character_,
    topology_tolerance,
    topology_tolerance_unit,
    native_horizontal_crs,
    native_vertical_datum = NA_character_,
    horizontal_unit,
    vertical_unit = NA_character_,
    cell_size = NA_real_,
    provenance_completeness,
    actor,
    created_at = Sys.time()) {
  observation_id <- .fg_uuid(
    stream_network_observation_id,
    "stream_network_observation_id"
  )
  configuration_id <- .fg_uuid(
    stream_network_configuration_id,
    "stream_network_configuration_id"
  )
  evidence_class <- .fg_choice(
    evidence_class,
    c(
      "DIRECT_TERRAIN_DERIVATION",
      "SOURCE_NETWORK_RETAINED",
      "RECONSTRUCTED_FROM_REACH_FLOWLINES"
    ),
    "evidence_class"
  )
  coverage_status <- .fg_choice(
    coverage_status,
    c("FULL_CONFIGURATION", "PARTIAL_CONFIGURATION", "KNOWN_GAPS", "UNKNOWN_LEGACY"),
    "coverage_status"
  )
  provenance_completeness <- .fg_choice(
    provenance_completeness,
    c("COMPLETE", "PARTIAL_LEGACY", "MINIMAL_LEGACY"),
    "provenance_completeness"
  )

  if (base::length(observation_year) != 1L || is.na(observation_year) ||
      observation_year != as.integer(observation_year) ||
      observation_year < 1000L || observation_year > 9999L) {
    .fg_abort("`observation_year` must be one four-digit integer.")
  }
  observation_year <- as.integer(observation_year)

  check_optional_integer <- function(x, name, minimum, maximum) {
    if (base::length(x) != 1L ||
        (!is.na(x) && (!is.numeric(x) || x != as.integer(x) ||
                       x < minimum || x > maximum))) {
      .fg_abort(sprintf("`%s` must be `NA` or an integer from %s through %s.",
                        name, minimum, maximum))
    }
    as.integer(x)
  }
  observation_month <- check_optional_integer(observation_month, "observation_month", 1L, 12L)
  observation_day <- check_optional_integer(observation_day, "observation_day", 1L, 31L)
  if (!is.na(observation_day) && is.na(observation_month)) {
    .fg_abort("`observation_day` requires `observation_month`.")
  }
  if (!is.na(observation_day)) {
    candidate_date <- as.Date(
      sprintf(
        "%04d-%02d-%02d",
        observation_year,
        observation_month,
        observation_day
      ),
      format = "%Y-%m-%d"
    )
    if (is.na(candidate_date) ||
        format(candidate_date, "%Y-%m-%d") != sprintf(
          "%04d-%02d-%02d",
          observation_year,
          observation_month,
          observation_day
        )) {
      .fg_abort("The observation year, month, and day do not form a valid date.")
    }
  }
  date_precision <- if (!is.na(observation_day)) {
    "DAY"
  } else if (!is.na(observation_month)) {
    "MONTH"
  } else {
    "YEAR"
  }

  source_terrain_id <- .fg_optional_uuid(source_terrain_id, "source_terrain_id")
  source_terrain_label <- .fg_optional_text(source_terrain_label, "source_terrain_label")
  source_terrain_fingerprint <- .fg_optional_text(
    source_terrain_fingerprint,
    "source_terrain_fingerprint"
  )
  if (!is.na(source_terrain_fingerprint) &&
      !grepl("^[0-9A-Fa-f]{64}$", source_terrain_fingerprint)) {
    .fg_abort("`source_terrain_fingerprint` must be a 64-character hexadecimal hash.")
  }

  derivation_method_id <- .fg_required_text(derivation_method_id, "derivation_method_id")
  method_version <- .fg_optional_text(method_version, "method_version")
  if (provenance_completeness == "COMPLETE" && is.na(method_version)) {
    .fg_abort("`method_version` is required when provenance is `COMPLETE`.")
  }

  threshold_value <- .fg_positive_optional(threshold_value, "threshold_value")
  threshold_unit <- .fg_optional_text(threshold_unit, "threshold_unit")
  if (xor(is.na(threshold_value), is.na(threshold_unit))) {
    .fg_abort("`threshold_value` and `threshold_unit` must be supplied together.")
  }

  topology_tolerance <- .fg_positive_optional(
    topology_tolerance,
    "topology_tolerance",
    required = TRUE
  )
  topology_tolerance_unit <- .fg_required_text(
    topology_tolerance_unit,
    "topology_tolerance_unit"
  )
  native_horizontal_crs <- .fg_optional_text(
    native_horizontal_crs,
    "native_horizontal_crs"
  )
  horizontal_unit <- .fg_optional_text(horizontal_unit, "horizontal_unit")
  if (provenance_completeness == "COMPLETE" &&
      (is.na(native_horizontal_crs) || is.na(horizontal_unit))) {
    .fg_abort("Complete provenance requires `native_horizontal_crs` and `horizontal_unit`.")
  }
  if (!is.na(horizontal_unit) && topology_tolerance_unit != horizontal_unit) {
    .fg_abort("`topology_tolerance_unit` must equal `horizontal_unit`.")
  }

  native_vertical_datum <- .fg_optional_text(
    native_vertical_datum,
    "native_vertical_datum"
  )
  vertical_unit <- .fg_optional_text(vertical_unit, "vertical_unit")
  if (xor(is.na(native_vertical_datum), is.na(vertical_unit))) {
    .fg_abort("`native_vertical_datum` and `vertical_unit` must be supplied together.")
  }

  cell_size <- .fg_positive_optional(
    cell_size,
    "cell_size",
    required = evidence_class == "DIRECT_TERRAIN_DERIVATION"
  )
  actor <- .fg_required_text(actor, "actor")
  created_at <- .fg_timestamp(created_at, "created_at")

  tibble::tibble(
    stream_network_observation_id = observation_id,
    stream_network_configuration_id = configuration_id,
    observation_year = observation_year,
    observation_month = observation_month,
    observation_day = observation_day,
    date_precision = date_precision,
    evidence_class = evidence_class,
    coverage_status = coverage_status,
    source_terrain_id = source_terrain_id,
    source_terrain_label = source_terrain_label,
    source_terrain_fingerprint = ifelse(
      is.na(source_terrain_fingerprint),
      NA_character_,
      tolower(source_terrain_fingerprint)
    ),
    derivation_method_id = derivation_method_id,
    method_version = method_version,
    threshold_value = threshold_value,
    threshold_unit = threshold_unit,
    topology_tolerance = topology_tolerance,
    topology_tolerance_unit = topology_tolerance_unit,
    native_horizontal_crs = native_horizontal_crs,
    native_vertical_datum = native_vertical_datum,
    horizontal_unit = horizontal_unit,
    vertical_unit = vertical_unit,
    cell_size = cell_size,
    provenance_completeness = provenance_completeness,
    review_status = "DRAFT",
    reviewed_at = as.POSIXct(NA, tz = "UTC"),
    reviewed_by = NA_character_,
    review_notes = NA_character_,
    created_at = created_at,
    created_by = actor,
    modified_at = created_at,
    modified_by = actor,
    lifecycle_status = "ACTIVE"
  )
}

#' Normalize a retained Stream Network
#'
#' Converts retained legacy Stream Network features into candidate governed
#' segment and source-lineage relations. Governed Stream and optional Reach
#' identities are supplied separately from the source features and are never
#' inferred from legacy names.
#'
#' This first normalization slice preserves source geometry and attributes. It
#' does not establish direction, governed topology-node identities, segment
#' role, analyst acceptance, or enterprise identity. Those fields remain
#' explicitly unresolved and are reported in the returned validation issues.
#'
#' @param stream_network Projected `sf` line features from a retained
#'   `stream_network` feature class.
#' @param source_mappings Data frame with exactly one row per source feature.
#'   `source_row` is the one-based row number in `stream_network`, `stream_id`
#'   is required, and `reach_id` is optional. Governed IDs are UUIDs.
#' @param configuration One `stream_network_configuration` row.
#' @param configuration_streams The corresponding
#'   `stream_network_configuration_stream` rows.
#' @param observation One `stream_network_observation` row whose evidence class
#'   is `SOURCE_NETWORK_RETAINED`.
#' @param actor Stable actor or process identifier.
#' @param source_dataset_name Reviewable source feature-class name.
#' @param created_at Creation date-time; converted to UTC.
#'
#' @return A named list containing `stream_network`, `stream_network_source`,
#'   `stream_network_validation_run`, and `stream_network_validation_issue`.
#' @export
#' @importFrom openssl rand_bytes
normalize_retained_stream_network <- function(
    stream_network,
    source_mappings,
    configuration,
    configuration_streams,
    observation,
    actor,
    source_dataset_name = "stream_network",
    created_at = Sys.time()) {
  if (!inherits(stream_network, "sf")) {
    .fg_abort("`stream_network` must be an `sf` object.")
  }
  if (!nrow(stream_network)) {
    .fg_abort("`stream_network` must contain at least one source feature.")
  }
  if (is.na(sf::st_crs(stream_network))) {
    .fg_abort("`stream_network` must have a coordinate reference system.")
  }
  if (isTRUE(sf::st_is_longlat(stream_network))) {
    .fg_abort("`stream_network` must use a projected coordinate reference system.")
  }

  geometry_type <- as.character(sf::st_geometry_type(stream_network))
  if (any(!geometry_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    .fg_abort("`stream_network` geometry must be LINESTRING or MULTILINESTRING.")
  }
  if (any(sf::st_is_empty(stream_network))) {
    .fg_abort("`stream_network` geometry cannot be empty.")
  }
  if (any(!sf::st_is_valid(stream_network))) {
    .fg_abort("`stream_network` geometry must be valid.")
  }
  if (any(as.numeric(sf::st_length(stream_network)) <= 0)) {
    .fg_abort("`stream_network` geometry must have positive length.")
  }

  if (!is.data.frame(configuration) || nrow(configuration) != 1L ||
      !all(c("stream_network_configuration_id", "study_area_id") %in%
           names(configuration))) {
    .fg_abort("`configuration` must contain one governed Configuration row.")
  }
  configuration_id <- .fg_uuid(
    configuration$stream_network_configuration_id,
    "configuration$stream_network_configuration_id"
  )

  membership_fields <- c("stream_network_configuration_id", "stream_id")
  if (!is.data.frame(configuration_streams) || !nrow(configuration_streams) ||
      !all(membership_fields %in% names(configuration_streams))) {
    .fg_abort("`configuration_streams` must contain governed Configuration-Stream rows.")
  }
  membership_configuration_ids <- .fg_uuid(
    configuration_streams$stream_network_configuration_id,
    "configuration_streams$stream_network_configuration_id"
  )
  if (any(membership_configuration_ids != configuration_id)) {
    .fg_abort("Every Configuration-Stream row must belong to `configuration`.")
  }
  member_stream_ids <- .fg_uuid(
    configuration_streams$stream_id,
    "configuration_streams$stream_id"
  )

  observation_fields <- c(
    "stream_network_observation_id",
    "stream_network_configuration_id",
    "evidence_class"
  )
  if (!is.data.frame(observation) || nrow(observation) != 1L ||
      !all(observation_fields %in% names(observation))) {
    .fg_abort("`observation` must contain one governed Observation row.")
  }
  observation_id <- .fg_uuid(
    observation$stream_network_observation_id,
    "observation$stream_network_observation_id"
  )
  observation_configuration_id <- .fg_uuid(
    observation$stream_network_configuration_id,
    "observation$stream_network_configuration_id"
  )
  if (observation_configuration_id != configuration_id) {
    .fg_abort("`observation` must belong to `configuration`.")
  }
  if (!identical(as.character(observation$evidence_class), "SOURCE_NETWORK_RETAINED")) {
    .fg_abort("`observation$evidence_class` must be `SOURCE_NETWORK_RETAINED`.")
  }

  mapping_fields <- c("source_row", "stream_id")
  if (!is.data.frame(source_mappings) ||
      !all(mapping_fields %in% names(source_mappings))) {
    .fg_abort("`source_mappings` must contain `source_row` and `stream_id`.")
  }
  if (nrow(source_mappings) != nrow(stream_network)) {
    .fg_abort("`source_mappings` must contain exactly one row per source feature.")
  }
  source_rows <- source_mappings$source_row
  if (!is.numeric(source_rows) || anyNA(source_rows) ||
      any(source_rows != as.integer(source_rows)) ||
      !setequal(as.integer(source_rows), seq_len(nrow(stream_network)))) {
    .fg_abort("`source_mappings$source_row` must identify every source row exactly once.")
  }
  if (anyDuplicated(source_rows)) {
    .fg_abort("`source_mappings$source_row` must be unique.")
  }
  source_mappings <- source_mappings[
    match(seq_len(nrow(stream_network)), as.integer(source_rows)),
    ,
    drop = FALSE
  ]
  mapped_stream_ids <- .fg_uuid(
    source_mappings$stream_id,
    "source_mappings$stream_id"
  )
  if (any(!mapped_stream_ids %in% member_stream_ids)) {
    .fg_abort("Every mapped Stream must participate in `configuration`.")
  }
  mapped_reach_ids <- if ("reach_id" %in% names(source_mappings)) {
    .fg_optional_uuid_vector(
      source_mappings$reach_id,
      "source_mappings$reach_id",
      nrow(source_mappings)
    )
  } else {
    rep(NA_character_, nrow(source_mappings))
  }

  actor <- .fg_required_text(actor, "actor")
  source_dataset_name <- .fg_required_text(
    source_dataset_name,
    "source_dataset_name"
  )
  created_at <- .fg_timestamp(created_at, "created_at")
  created_at_value <- created_at

  source <- stream_network
  source$.source_row <- seq_len(nrow(source))
  source$.source_part_count <- ifelse(
    geometry_type == "MULTILINESTRING",
    lengths(sf::st_geometry(source)),
    1L
  )
  normalized <- suppressWarnings(sf::st_cast(source, "LINESTRING"))
  normalized <- sf::st_zm(normalized, drop = TRUE, what = "ZM")
  normalized$.source_part <- ave(
    normalized$.source_row,
    normalized$.source_row,
    FUN = seq_along
  )
  normalized$.stream_id <- mapped_stream_ids[normalized$.source_row]
  normalized$.reach_id <- mapped_reach_ids[normalized$.source_row]

  segment_ids <- .fg_generate_uuid(nrow(normalized))
  source_ids <- .fg_generate_uuid(nrow(normalized))
  validation_run_id <- .fg_generate_uuid(1L)
  source_feature_key <- .fg_source_text(normalized, "arcid")

  segments <- sf::st_sf(
    tibble::tibble(
      stream_network_segment_id = segment_ids,
      stream_network_observation_id = rep(observation_id, nrow(normalized)),
      stream_id = normalized$.stream_id,
      reach_id = normalized$.reach_id,
      downstream_node_id = rep(NA_character_, nrow(normalized)),
      upstream_node_id = rep(NA_character_, nrow(normalized)),
      segment_role = rep("UNRESOLVED", nrow(normalized)),
      direction_status = rep("UNRESOLVED", nrow(normalized)),
      direction_method = rep("LEGACY_UNKNOWN", nrow(normalized)),
      source_feature_key = source_feature_key,
      review_status = rep("PENDING", nrow(normalized)),
      created_at = rep(created_at_value, nrow(normalized)),
      created_by = rep(actor, nrow(normalized)),
      modified_at = rep(created_at_value, nrow(normalized)),
      modified_by = rep(actor, nrow(normalized)),
      lifecycle_status = rep("ACTIVE", nrow(normalized))
    ),
    Shape = sf::st_geometry(normalized)
  )

  sources <- tibble::tibble(
    stream_network_source_id = source_ids,
    stream_network_segment_id = segment_ids,
    source_object_type = "RETAINED_STREAM_NETWORK",
    source_object_id = NA_character_,
    source_dataset_name = source_dataset_name,
    source_feature_key = source_feature_key,
    source_from_node_key = .fg_source_text(normalized, "from_node"),
    source_to_node_key = .fg_source_text(normalized, "to_node"),
    source_class_code = .fg_source_text(normalized, "grid_code"),
    source_reach_name = .fg_source_text(normalized, "ReachName"),
    relation_code = "GEOMETRY_SOURCE",
    geometry_modified = normalized$.source_part_count > 1L
  )

  validation_run <- tibble::tibble(
    stream_network_validation_run_id = validation_run_id,
    stream_network_observation_id = observation_id,
    validation_level = "WORKING",
    result = "REVIEW_REQUIRED",
    model_version = "FGDB_STREAM_NETWORK_1",
    validator_version = "RETAINED_NORMALIZATION_0.1",
    validated_at = created_at,
    validated_by = actor
  )

  validation_issue <- tibble::tibble(
    stream_network_validation_issue_id = .fg_generate_uuid(nrow(normalized)),
    stream_network_validation_run_id = rep(validation_run_id, nrow(normalized)),
    issue_code = "SEGMENT_REVIEW_REQUIRED",
    severity = "ERROR",
    affected_relation = "stream_network",
    affected_object_id = segment_ids,
    related_relation = "stream_network_source",
    related_object_id = source_ids,
    message = paste(
      "Retained geometry requires reviewed direction, node identities,",
      "and segment role before acceptance."
    ),
    analyst_disposition = "UNRESOLVED",
    disposition_at = as.POSIXct(NA, tz = "UTC"),
    disposition_by = NA_character_,
    disposition_notes = NA_character_
  )

  list(
    stream_network = segments,
    stream_network_source = sources,
    stream_network_validation_run = validation_run,
    stream_network_validation_issue = validation_issue
  )
}
