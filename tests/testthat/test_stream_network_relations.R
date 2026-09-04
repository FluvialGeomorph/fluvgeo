stream_network_test_ids <- list(
  configuration = "11111111-1111-4111-8111-111111111111",
  study_area = "22222222-2222-4222-8222-222222222222",
  stream_1 = "33333333-3333-4333-8333-333333333333",
  stream_2 = "44444444-4444-4444-8444-444444444444",
  observation = "55555555-5555-4555-8555-555555555555",
  reach = "66666666-6666-4666-8666-666666666666"
)

stream_network_test_context <- function(source_network) {
  configuration <- create_stream_network_configuration(
    stream_network_configuration_id = stream_network_test_ids$configuration,
    study_area_id = stream_network_test_ids$study_area,
    configuration_name = "Retained network test",
    configuration_mode = "STREAM",
    streams = data.frame(
      stream_id = stream_network_test_ids$stream_1,
      stream_name = unique(source_network$ReachName)[1]
    ),
    actor = "testthat",
    created_at = as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
  )
  observation <- create_stream_network_observation(
    stream_network_observation_id = stream_network_test_ids$observation,
    stream_network_configuration_id = stream_network_test_ids$configuration,
    observation_year = 2013L,
    evidence_class = "SOURCE_NETWORK_RETAINED",
    coverage_status = "UNKNOWN_LEGACY",
    source_terrain_label = "retained-test.gdb",
    derivation_method_id = "LEGACY_UNKNOWN",
    topology_tolerance = 0.01,
    topology_tolerance_unit = "METRE",
    native_horizontal_crs = sf::st_crs(source_network)$input,
    horizontal_unit = "METRE",
    provenance_completeness = "PARTIAL_LEGACY",
    actor = "testthat",
    created_at = as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
  )

  list(
    configuration = configuration$stream_network_configuration,
    configuration_streams =
      configuration$stream_network_configuration_stream,
    observation = observation
  )
}

test_that("configuration relations use direct retained Stream names", {
  source_gdb <- system.file(
    "extdata",
    "testing_data.gdb",
    package = "fluvgeodata"
  )
  source_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )
  source_name <- unique(source_network$ReachName)

  result <- create_stream_network_configuration(
    stream_network_configuration_id = stream_network_test_ids$configuration,
    study_area_id = stream_network_test_ids$study_area,
    configuration_name = source_name,
    configuration_mode = "STREAM",
    streams = data.frame(
      stream_id = stream_network_test_ids$stream_1,
      stream_name = source_name
    ),
    actor = "testthat",
    created_at = as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
  )

  expect_named(
    result,
    c("stream_network_configuration", "stream_network_configuration_stream")
  )
  expect_s3_class(result$stream_network_configuration, "tbl_df")
  expect_s3_class(result$stream_network_configuration_stream, "tbl_df")
  expect_equal(nrow(result$stream_network_configuration), 1L)
  expect_equal(nrow(result$stream_network_configuration_stream), 1L)
  expect_equal(
    result$stream_network_configuration_stream$stream_name,
    source_name
  )
  expect_equal(result$stream_network_configuration$lifecycle_status, "ACTIVE")
})

test_that("configuration cardinality follows its modeled object", {
  streams <- data.frame(
    stream_id = c(
      stream_network_test_ids$stream_1,
      stream_network_test_ids$stream_2
    ),
    stream_name = c("Stream one", "Stream two")
  )

  expect_error(
    create_stream_network_configuration(
      stream_network_test_ids$configuration,
      stream_network_test_ids$study_area,
      "Invalid single Stream configuration",
      "STREAM",
      streams,
      actor = "testthat"
    ),
    "exactly one Stream"
  )

  result <- create_stream_network_configuration(
    stream_network_test_ids$configuration,
    stream_network_test_ids$study_area,
    "Connected Streams",
    "STUDY_AREA_NETWORK",
    streams,
    actor = "testthat"
  )
  expect_equal(nrow(result$stream_network_configuration_stream), 2L)
})

test_that("retained-network observation represents incomplete direct evidence", {
  source_gdb <- system.file(
    "extdata",
    "AntelopeCreek_2013.gdb",
    package = "fluvgeodata"
  )
  source_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )

  result <- create_stream_network_observation(
    stream_network_observation_id = stream_network_test_ids$observation,
    stream_network_configuration_id = stream_network_test_ids$configuration,
    observation_year = 2013L,
    evidence_class = "SOURCE_NETWORK_RETAINED",
    coverage_status = "UNKNOWN_LEGACY",
    source_terrain_label = basename(source_gdb),
    derivation_method_id = "LEGACY_UNKNOWN",
    topology_tolerance = 0.01,
    topology_tolerance_unit = "METRE",
    native_horizontal_crs = sf::st_crs(source_network)$input,
    horizontal_unit = "METRE",
    provenance_completeness = "PARTIAL_LEGACY",
    actor = "testthat",
    created_at = as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$date_precision, "YEAR")
  expect_equal(result$review_status, "DRAFT")
  expect_equal(result$source_terrain_label, "AntelopeCreek_2013.gdb")
  expect_true(is.na(result$method_version))
})

test_that("observation constructor rejects relationally invalid states", {
  common <- list(
    stream_network_observation_id = stream_network_test_ids$observation,
    stream_network_configuration_id = stream_network_test_ids$configuration,
    observation_year = 2024L,
    evidence_class = "DIRECT_TERRAIN_DERIVATION",
    coverage_status = "FULL_CONFIGURATION",
    derivation_method_id = "fluvgeo::derive_stream_network_from_terrain",
    method_version = "1.0.0",
    topology_tolerance = 0.01,
    topology_tolerance_unit = "METRE",
    native_horizontal_crs = "EPSG:26914",
    horizontal_unit = "METRE",
    cell_size = 1,
    provenance_completeness = "COMPLETE",
    actor = "testthat"
  )

  invalid_date <- common
  invalid_date$observation_month <- 2L
  invalid_date$observation_day <- 30L
  expect_error(
    do.call(create_stream_network_observation, invalid_date),
    "valid date"
  )

  missing_cell_size <- common
  missing_cell_size$cell_size <- NA_real_
  expect_error(
    do.call(create_stream_network_observation, missing_cell_size),
    "cell_size.*required"
  )

  mismatched_unit <- common
  mismatched_unit$topology_tolerance_unit <- "FOOT"
  expect_error(
    do.call(create_stream_network_observation, mismatched_unit),
    "must equal"
  )
})

test_that("retained features normalize into candidate segments and lineage", {
  source_gdb <- system.file(
    "extdata",
    "testing_data.gdb",
    package = "fluvgeodata"
  )
  source_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )
  context <- stream_network_test_context(source_network)
  mappings <- data.frame(
    source_row = seq_len(nrow(source_network)),
    stream_id = stream_network_test_ids$stream_1,
    reach_id = stream_network_test_ids$reach
  )

  result <- normalize_retained_stream_network(
    stream_network = source_network,
    source_mappings = mappings,
    configuration = context$configuration,
    configuration_streams = context$configuration_streams,
    observation = context$observation,
    actor = "testthat",
    source_dataset_name = "stream_network",
    created_at = as.POSIXct("2026-09-03 12:00:00", tz = "UTC")
  )

  expect_named(
    result,
    c(
      "stream_network",
      "stream_network_source",
      "stream_network_validation_run",
      "stream_network_validation_issue"
    )
  )
  expect_s3_class(result$stream_network, "sf")
  expect_equal(nrow(result$stream_network), nrow(source_network))
  expect_true(all(sf::st_geometry_type(result$stream_network) == "LINESTRING"))
  expect_true(all(result$stream_network$stream_id == stream_network_test_ids$stream_1))
  expect_true(all(result$stream_network$reach_id == stream_network_test_ids$reach))
  expect_true(all(result$stream_network$direction_status == "UNRESOLVED"))
  expect_true(all(result$stream_network$review_status == "PENDING"))
  expect_true(all(grepl(.fg_uuid_pattern, result$stream_network$stream_network_segment_id)))

  expect_equal(
    result$stream_network_source$source_feature_key,
    as.character(source_network$arcid)
  )
  expect_equal(
    result$stream_network_source$source_from_node_key,
    as.character(source_network$from_node)
  )
  expect_equal(
    result$stream_network_source$source_to_node_key,
    as.character(source_network$to_node)
  )
  expect_equal(
    result$stream_network_source$source_class_code,
    as.character(source_network$grid_code)
  )
  expect_equal(
    result$stream_network_source$source_reach_name,
    as.character(source_network$ReachName)
  )
  expect_false(any(result$stream_network_source$geometry_modified))
  expect_equal(result$stream_network_validation_run$result, "REVIEW_REQUIRED")
  expect_equal(
    nrow(result$stream_network_validation_issue),
    nrow(result$stream_network)
  )
})

test_that("normalization preserves explicitly missing legacy source values", {
  source_gdb <- system.file(
    "extdata",
    "y2006_R1.gdb",
    package = "fluvgeodata"
  )
  source_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )
  context <- stream_network_test_context(source_network)

  result <- normalize_retained_stream_network(
    stream_network = source_network,
    source_mappings = data.frame(
      source_row = 1L,
      stream_id = stream_network_test_ids$stream_1
    ),
    configuration = context$configuration,
    configuration_streams = context$configuration_streams,
    observation = context$observation,
    actor = "testthat"
  )

  expect_true(is.na(result$stream_network$reach_id))
  expect_true(is.na(result$stream_network_source$source_feature_key))
  expect_true(is.na(result$stream_network_source$source_from_node_key))
  expect_true(is.na(result$stream_network_source$source_to_node_key))
  expect_true(is.na(result$stream_network_source$source_class_code))
  expect_equal(
    result$stream_network_source$source_reach_name,
    source_network$ReachName
  )
})

test_that("multipart retained features create traceable candidate parts", {
  source_gdb <- system.file(
    "extdata",
    "testing_data.gdb",
    package = "fluvgeodata"
  )
  direct_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )
  source_network <- direct_network[1, ]
  source_geometry <- sf::st_geometry(direct_network[1:2, ])
  line_parts <- lapply(source_geometry, function(x) x[[1]])
  sf::st_geometry(source_network) <- sf::st_sfc(
    sf::st_multilinestring(line_parts),
    crs = sf::st_crs(direct_network)
  )
  context <- stream_network_test_context(source_network)

  result <- normalize_retained_stream_network(
    stream_network = source_network,
    source_mappings = data.frame(
      source_row = 1L,
      stream_id = stream_network_test_ids$stream_1
    ),
    configuration = context$configuration,
    configuration_streams = context$configuration_streams,
    observation = context$observation,
    actor = "testthat"
  )

  expect_equal(nrow(result$stream_network), 2L)
  expect_equal(nrow(result$stream_network_source), 2L)
  expect_true(all(result$stream_network_source$geometry_modified))
  expect_equal(length(unique(result$stream_network_source$source_feature_key)), 1L)
})

test_that("normalization rejects incomplete or foreign mappings", {
  source_gdb <- system.file(
    "extdata",
    "testing_data.gdb",
    package = "fluvgeodata"
  )
  source_network <- suppressWarnings(
    sf::st_read(source_gdb, layer = "stream_network", quiet = TRUE)
  )[1:2, ]
  context <- stream_network_test_context(source_network)

  expect_error(
    normalize_retained_stream_network(
      source_network,
      data.frame(source_row = 1L, stream_id = stream_network_test_ids$stream_1),
      context$configuration,
      context$configuration_streams,
      context$observation,
      actor = "testthat"
    ),
    "exactly one row"
  )

  expect_error(
    normalize_retained_stream_network(
      source_network,
      data.frame(
        source_row = 1:2,
        stream_id = stream_network_test_ids$stream_2
      ),
      context$configuration,
      context$configuration_streams,
      context$observation,
      actor = "testthat"
    ),
    "must participate"
  )
})
