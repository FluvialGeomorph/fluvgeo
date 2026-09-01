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
