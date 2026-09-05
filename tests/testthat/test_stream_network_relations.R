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

retained_network_test_source <- function() {
  suppressWarnings(sf::st_read(
    system.file("extdata", "testing_data.gdb", package = "fluvgeodata"),
    layer = "stream_network", quiet = TRUE
  ))
}

prepare_test_network <- function(source, mode = "CREATE_REVIEW_FEATURES",
                                 tolerance = 0.01, unit = "METRE", dem = NULL,
                                 consolidate = FALSE, protected_nodes = NULL) {
  context <- stream_network_test_context(source)
  context$observation$topology_tolerance <- tolerance
  context$observation$topology_tolerance_unit <- unit
  prepare_stream_network_from_features(
    source, data.frame(source_row = seq_len(nrow(source)),
                       stream_id = stream_network_test_ids$stream_1),
    context$configuration, context$configuration_streams,
    context$observation, actor = "testthat", review_mode = mode, dem = dem,
    consolidate = consolidate, protected_nodes = protected_nodes
  )
}

test_that("preparation consolidates before direction with many-source operation lineage", {
  # Explicit synthetic geometry and raster: the middle short line occupies one
  # cell; the logical link has measurable relief. A separate singleton catches
  # accidental row-index assumptions in source updates.
  coords <- list(c(0.2, 1.2), c(1.2, 1.4), c(1.4, 2.2), c(10.2, 11.2))
  source <- sf::st_sf(arcid = 1:4, ReachName = "Synthetic",
    geometry = sf::st_sfc(lapply(coords, function(x) {
      sf::st_linestring(cbind(x, 0.5))
    }), crs = 26915))
  dem <- terra::rast(nrows = 1, ncols = 12, xmin = 0, xmax = 12,
                     ymin = 0, ymax = 1, crs = "EPSG:26915")
  terra::values(dem) <- 12:1
  before <- source
  raw <- prepare_test_network(source, dem = dem)
  expect_equal(sum(raw$stream_network_validation_issue$issue_code == "DIRECTION_UNRESOLVED"), 1L)
  out <- prepare_test_network(source, dem = dem, consolidate = TRUE)
  expect_identical(source, before)
  expect_equal(nrow(out$stream_network), 2L)
  expect_true(all(out$stream_network$direction_status == "CONFIRMED"))
  expect_equal(out$stream_network_validation_issue$issue_code, rep("SEGMENT_REVIEW_REQUIRED", 2))
  expect_equal(out$stream_network_validation_run$result, "REVIEW_REQUIRED")
  expect_true(all(is.na(out$stream_network$downstream_node_id)))
  lineage <- out$stream_network_source
  expect_equal(lineage$source_feature_key, as.character(1:4))
  expect_equal(lineage$stream_network_segment_id,
               out$stream_network$stream_network_segment_id[c(1,1,1,2)])
  expect_true(all(lineage$geometry_modified))
  expect_true(is.na(out$stream_network$source_feature_key[1]))
  expect_equal(out$stream_network$source_feature_key[2], "4")
  ops <- out$stream_network_operation
  expect_equal(ops$operation_code, c("CONSOLIDATE_SEGMENTS", "REVERSE_DIRECTION", "REVERSE_DIRECTION"))
  expect_equal(ops$operation_sequence, c(1L, 2L, 1L))
  expect_true(all(is.na(ops$stream_network_source_id[1:2])))
  expect_equal(ops$stream_network_source_id[3], lineage$stream_network_source_id[4])
  expect_equal(out$stream_network_direction_evidence$stream_network_operation_id,
               ops$stream_network_operation_id[2:3])
  expect_true(is.na(out$stream_network_review$stream_network_source_id[1]))
  expect_equal(out$stream_network_review$stream_network_source_id[2], lineage$stream_network_source_id[4])
  expect_true(is.na(out$stream_network_validation_issue$related_relation[1]))
  expect_equal(nrow(prepare_test_network(source, consolidate = TRUE)$stream_network_operation), 1L)
  flat <- terra::init(dem, 1)
  expect_true(all(prepare_test_network(source, dem = flat, consolidate = TRUE)$
                   stream_network_direction_evidence$action == "UNRESOLVED"))
  preview <- prepare_test_network(source, "VALIDATE_ONLY", dem = dem, consolidate = TRUE)
  expect_equal(nrow(preview$stream_network), 4L)
  expect_equal(nrow(preview$stream_network_operation), 0L)
  expect_identical(sf::st_geometry(preview$stream_network), sf::st_geometry(source))
  # Interior raw endpoint NoData cannot be hidden by removing that endpoint.
  terra::values(dem)[2] <- NA_real_
  expect_error(prepare_test_network(source, dem = dem, consolidate = TRUE), "coverage is incomplete")
})

test_that("preparation preserves multipart lineage and explicit boundaries", {
  source <- retained_network_test_source()
  out <- prepare_test_network(source, consolidate = TRUE)
  expect_equal(nrow(out$stream_network), 1L)
  expect_equal(nrow(out$stream_network_source), 99L)
  expect_equal(out$stream_network_source$source_feature_key, as.character(source$arcid))
  expect_true(all(out$stream_network_source$geometry_modified))
  expect_equal(out$stream_network_operation$operation_code, "CONSOLIDATE_SEGMENTS")
  p <- sf::st_geometry(suppressWarnings(sf::st_cast(source[source$arcid == 1126,], "LINESTRING")))[[1]]
  point <- sf::st_sfc(sf::st_point(p[nrow(p),]), crs = sf::st_crs(source))
  protected <- prepare_test_network(source, consolidate = TRUE, protected_nodes = point)
  expect_equal(nrow(protected$stream_network), 2L)
  expect_equal(nrow(protected$stream_network_source), 99L)
  # Merge two parts of a single retained feature without dropping either source relation.
  multi <- source[1,]
  sf::st_geometry(multi) <- sf::st_sfc(sf::st_multilinestring(list(
    rbind(c(0,0),c(1,0)), rbind(c(1,0),c(2,0)))), crs = sf::st_crs(source))
  joined <- prepare_test_network(multi, consolidate = TRUE)
  expect_equal(nrow(joined$stream_network), 1L)
  expect_equal(nrow(joined$stream_network_source), 2L)
  expect_equal(length(unique(joined$stream_network_source$stream_network_source_id)), 2L)
})

test_that("DEM preparation resolves direction with linked automatic operations", {
  source <- retained_network_test_source()
  dem <- terra::rast(system.file("extdata", "dem_1m.tif", package = "fluvgeodata"))
  # This raster covers only part of the network. Reject the full pair, then
  # explicitly test a covered subset; never present it as a full-source DEM.
  expect_error(prepare_test_network(source, dem = dem), "46 segments outside its extent; 4 additional")
  preview <- prepare_test_network(source, "VALIDATE_ONLY", dem = dem)
  expect_equal(sum(preview$stream_network_validation_issue$issue_code == "DEM_COVERAGE_INCOMPLETE"), 50L)
  expect_equal(nrow(preview$stream_network_operation), 0L)
  sampled <- preview$stream_network_direction_evidence
  source <- source[sampled$start_sample_status == "AVAILABLE" &
                     sampled$end_sample_status == "AVAILABLE", ]
  expect_equal(nrow(source), 49L)
  before <- source
  result <- prepare_test_network(source, dem = dem)
  segments <- result$stream_network
  evidence <- result$stream_network_direction_evidence
  operations <- result$stream_network_operation
  issues <- result$stream_network_validation_issue
  supported <- evidence$action != "UNRESOLVED"
  reversed <- evidence$action == "REVERSE"
  expect_identical(source, before)
  expect_equal(sum(supported), 46L)
  expect_equal(sum(reversed), 33L)
  expect_equal(sum(segments$direction_status == "CONFIRMED"), 46L)
  expect_true(all(segments$direction_method[supported] == "TERRAIN_ELEVATION"))
  expect_equal(sum(issues$issue_code == "DIRECTION_UNRESOLVED"), 3L)
  expect_equal(sum(issues$issue_code == "SEGMENT_REVIEW_REQUIRED"), 46L)
  expect_equal(nrow(operations), 46L)
  expect_equal(sum(operations$operation_code == "REVERSE_DIRECTION"), 33L)
  expect_equal(sum(operations$operation_code == "CONFIRM_DIRECTION"), 13L)
  expect_equal(evidence$stream_network_operation_id[supported], operations$stream_network_operation_id)
  expect_equal(evidence$stream_network_segment_id, segments$stream_network_segment_id)
  expect_equal(operations$stream_network_segment_id, segments$stream_network_segment_id[supported])
  expect_equal(operations$stream_network_source_id,
               result$stream_network_source$stream_network_source_id[supported])
  expect_true(all(operations$performed_by == "testthat"))
  expect_equal(result$stream_network_source$geometry_modified, reversed)
  # Only coordinate order changes: every candidate still covers its source line.
  expect_true(all(vapply(sf::st_equals(segments, source), length, integer(1)) >= 1L))
  expect_true(all(is.na(segments$downstream_node_id)))
  expect_true(all(segments$review_status == "PENDING"))
  expect_equal(result$stream_network_validation_run$result, "REVIEW_REQUIRED")

  validate <- prepare_test_network(source, "VALIDATE_ONLY", dem = dem)
  normalized <- suppressWarnings(sf::st_cast(source, "LINESTRING"))
  expect_equal(sf::st_geometry(validate$stream_network), sf::st_geometry(normalized))
  expect_equal(nrow(validate$stream_network_operation), 0L)
  expect_equal(nrow(validate$stream_network_review), 0L)
  expect_true(all(is.na(validate$stream_network_direction_evidence$stream_network_operation_id)))
  expect_true(all(validate$stream_network$direction_status == "UNRESOLVED"))
  expect_equal(validate$stream_network_direction_evidence$action, evidence$action)
  no_dem <- prepare_test_network(source)
  expect_equal(nrow(no_dem$stream_network_direction_evidence), 0L)
  expect_equal(nrow(no_dem$stream_network_operation), 0L)
  expect_identical(vapply(no_dem$stream_network_operation, typeof, character(1)),
                   vapply(operations, typeof, character(1)))
})

test_that("retained assessment produces linked pending spatial inspection rows", {
  source <- retained_network_test_source()
  before <- source
  result <- prepare_test_network(source)
  issues <- result$stream_network_validation_issue
  review <- result$stream_network_review
  segments <- result$stream_network
  expect_identical(source, before)
  expect_equal(nrow(segments), 99L)
  expect_equal(sum(issues$issue_code == "DIRECTION_UNRESOLVED"), 99L)
  expect_equal(review$stream_network_validation_issue_id,
               issues$stream_network_validation_issue_id)
  expect_equal(review$reason_code, issues$issue_code)
  expect_true(all(review$decision == "PENDING"))
  expect_true(all(review$operation_code == "INSPECT"))
  expect_true(all(is.na(review$proposed_node_id)))
  expect_true(all(segments$direction_status == "UNRESOLVED"))
  expect_true(all(is.na(segments$downstream_node_id)))
  expect_equal(sf::st_geometry(review), sf::st_geometry(segments)[
    match(review$stream_network_segment_id, segments$stream_network_segment_id)
  ])
  expect_equal(result$stream_network_validation_run$result, "REVIEW_REQUIRED")
  expect_equal(sf::st_crs(review), sf::st_crs(source))

  tables_only <- prepare_test_network(source, "VALIDATE_ONLY")
  expect_equal(nrow(tables_only$stream_network_review), 0L)
  expect_s3_class(tables_only$stream_network_review, "sf")
  expect_equal(names(tables_only$stream_network_review), names(review))
  expect_equal(tables_only$stream_network_validation_issue$issue_code, issues$issue_code)
})

test_that("reversed duplicates remain unchanged and reference both segments", {
  source <- retained_network_test_source()[c(1L, 1L), ]
  line <- sf::st_geometry(source)[[1L]][[1L]]
  sf::st_geometry(source) <- sf::st_sfc(
    sf::st_linestring(line), sf::st_linestring(line[nrow(line):1L, ]),
    crs = sf::st_crs(source)
  )
  result <- prepare_test_network(source)
  duplicate <- subset(result$stream_network_validation_issue,
                      issue_code == "DUPLICATE_GEOMETRY")
  expect_equal(nrow(duplicate), 1L)
  expect_equal(duplicate$affected_object_id,
               result$stream_network$stream_network_segment_id[1L])
  expect_equal(duplicate$related_object_id,
               result$stream_network$stream_network_segment_id[2L])
  expect_equal(sf::st_geometry(result$stream_network), sf::st_geometry(source))
})

test_that("endpoint tolerance distinguishes exact joins, near misses and gaps", {
  source <- retained_network_test_source()[c(1L, 1L), ]
  # Controlled in-memory splits of the first retained line edge.
  edge <- sf::st_geometry(source)[[1L]][[1L]][1:2, ]
  midpoint <- colMeans(edge)
  direction <- (edge[2L, ] - edge[1L, ]) / sqrt(sum((edge[2L, ] - edge[1L, ])^2))
  make_gap <- function(gap) {
    changed <- source
    sf::st_geometry(changed) <- sf::st_sfc(
      sf::st_linestring(rbind(edge[1L, ], midpoint)),
      sf::st_linestring(rbind(midpoint + gap * direction, edge[2L, ])),
      crs = sf::st_crs(source)
    )
    changed
  }
  exact <- prepare_test_network(make_gap(0))
  near <- prepare_test_network(make_gap(0.005))
  far <- prepare_test_network(make_gap(0.02))
  expect_false("ENDPOINT_NEAR_MISS" %in% exact$stream_network_validation_issue$issue_code)
  expect_false("INTERIOR_INTERSECTION" %in% exact$stream_network_validation_issue$issue_code)
  expect_equal(sum(near$stream_network_validation_issue$issue_code == "ENDPOINT_NEAR_MISS"), 1L)
  expect_false("ENDPOINT_NEAR_MISS" %in% far$stream_network_validation_issue$issue_code)
  expect_equal(sf::st_geometry(near$stream_network), sf::st_geometry(make_gap(0.005)))
})

test_that("interior junctions are presented for inspection without splitting", {
  source <- retained_network_test_source()[c(1L, 1L), ]
  edge <- sf::st_geometry(source)[[1L]][[1L]][1:2, ]
  midpoint <- colMeans(edge)
  delta <- edge[2L, ] - edge[1L, ]
  perpendicular <- c(-delta[2L], delta[1L])
  sf::st_geometry(source) <- sf::st_sfc(
    sf::st_linestring(edge),
    sf::st_linestring(rbind(midpoint, midpoint + perpendicular)),
    crs = sf::st_crs(source)
  )
  result <- prepare_test_network(source)
  expect_equal(sum(result$stream_network_validation_issue$issue_code == "INTERIOR_INTERSECTION"), 1L)
  expect_equal(nrow(result$stream_network), 2L)
  expect_equal(sf::st_geometry(result$stream_network), sf::st_geometry(source))
})

test_that("assessment rejects invalid tolerances and mismatched CRS units", {
  source <- retained_network_test_source()[1L, ]
  expect_error(prepare_test_network(source, tolerance = 0), "must be positive")
  expect_error(prepare_test_network(source, tolerance = NA_real_), "is required")
  expect_error(prepare_test_network(source, unit = "FOOT"), "must match the projected CRS")
  expect_error(prepare_test_network(source, unit = "UNSPECIFIED"), "must match the projected CRS")
})

test_that("closed and self-intersecting retained edits require inspection", {
  source <- retained_network_test_source()[1L, ]
  edge <- sf::st_geometry(source)[[1L]][[1L]][1:2, ]
  a <- edge[1L, ]
  b <- edge[2L, ]
  delta <- b - a
  perpendicular <- c(-delta[2L], delta[1L])
  sf::st_geometry(source) <- sf::st_sfc(sf::st_linestring(rbind(
    a, b + perpendicular, a + perpendicular, b, a
  )), crs = sf::st_crs(source))
  result <- prepare_test_network(source)
  expect_true(all(c("CLOSED_SEGMENT", "SELF_INTERSECTION") %in%
                    result$stream_network_validation_issue$issue_code))
})

test_that("preparation accepts explicitly unassigned Reach mappings", {
  source <- retained_network_test_source()[1L, ]
  context <- stream_network_test_context(source)
  result <- prepare_stream_network_from_features(
    source, data.frame(source_row = 1L,
                       stream_id = stream_network_test_ids$stream_1,
                       reach_id = NA),
    context$configuration, context$configuration_streams,
    context$observation, actor = "testthat"
  )
  expect_true(is.na(result$stream_network$reach_id))
})

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
