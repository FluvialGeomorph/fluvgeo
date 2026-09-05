acceptance_test_fixture <- function() {
  source <- sf::st_sf(arcid = 1:2, ReachName = "Synthetic",
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0.2,0.5), c(1.2,0.5))),
                          sf::st_linestring(rbind(c(1.2,0.5), c(2.2,0.5))), crs = 26915))
  config <- create_stream_network_configuration(.fg_generate_uuid(1), .fg_generate_uuid(1),
    "Synthetic", "STREAM", data.frame(stream_id = .fg_generate_uuid(1), stream_name = "Synthetic"), actor = "fixture")
  obs <- create_stream_network_observation(.fg_generate_uuid(1), config$stream_network_configuration$stream_network_configuration_id,
    observation_year = 2026L, evidence_class = "SOURCE_NETWORK_RETAINED", coverage_status = "PARTIAL_CONFIGURATION",
    derivation_method_id = "SYNTHETIC_TEST", topology_tolerance = 0.01, topology_tolerance_unit = "METRE",
    native_horizontal_crs = "EPSG:26915", horizontal_unit = "METRE", provenance_completeness = "PARTIAL_LEGACY", actor = "fixture")
  dem <- terra::rast(nrows = 1, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 1, crs = "EPSG:26915")
  terra::values(dem) <- 3:1
  p <- prepare_stream_network_from_features(source,
    data.frame(source_row = 1:2, stream_id = config$stream_network_configuration_stream$stream_id),
    config$stream_network_configuration, config$stream_network_configuration_stream, obs,
    actor = "fixture", dem = dem, consolidate = TRUE, connect = TRUE)
  list(p = p, config = config, obs = obs)
}

acceptance_test_validate <- function(f, level = "ACCEPTANCE", ...) {
  validate_stream_network(f$config$stream_network_configuration,
    f$config$stream_network_configuration_stream, f$obs, f$p$stream_network,
    sources = f$p$stream_network_source, operations = f$p$stream_network_operation,
    nodes = f$p$stream_network_node, connections = f$p$stream_network_connection,
    review_features = f$p$stream_network_review, level = level, actor = "test-validator", ...)
}

acceptance_test_classify <- function(f) {
  f$p <- classify_stream_network_segments(f$p, data.frame(
    stream_network_segment_id = f$p$stream_network$stream_network_segment_id,
    segment_role = "MAINSTEM", decision_notes = "Synthetic fixture explicitly represents a mainstem."), actor = "fixture")
  f
}

acceptance_test_review <- function(f) {
  # Explicit synthetic review, NOT an approval of a real Observation.
  f <- acceptance_test_classify(f)
  f$p$stream_network_review$decision <- "ACCEPT"
  f$p$stream_network_review$decision_by <- "synthetic-reviewer"
  f$p$stream_network_review$decision_at <- Sys.time() + 1
  f$p$stream_network_review$decision_notes <- "Synthetic geometry and attributes reviewed for this test."
  f$obs$review_notes <- "Synthetic partial-coverage fixture with intentionally partial legacy provenance."
  f
}

test_that("classification preserves geometry, lineage, reviews, and existing history", {
  f <- acceptance_test_fixture()
  before <- f$p
  f <- acceptance_test_classify(f)
  expect_equal(f$p$stream_network$segment_role, "MAINSTEM")
  expect_identical(sf::st_geometry(f$p$stream_network), sf::st_geometry(before$stream_network))
  expect_identical(f$p$stream_network_source, before$stream_network_source)
  expect_identical(f$p$stream_network_node, before$stream_network_node)
  expect_identical(f$p$stream_network_review, before$stream_network_review)
  expect_identical(f$p$stream_network_validation_issue, before$stream_network_validation_issue)
  expect_equal(tail(f$p$stream_network_operation$operation_code, 1), "CLASSIFY_SEGMENT_ROLE")
  expect_equal(tail(f$p$stream_network_operation$operation_sequence, 1), 4L)
  expect_equal(tail(f$p$stream_network_operation$segment_role, 1), "MAINSTEM")
  expect_true(is.na(tail(f$p$stream_network_operation$stream_network_source_id, 1)))
  expect_identical(acceptance_test_classify(f)$p, f$p)
  decisions <- data.frame(stream_network_segment_id = f$p$stream_network$stream_network_segment_id,
                          segment_role = "INVALID", decision_notes = "test")
  expect_error(classify_stream_network_segments(f$p, decisions, "fixture"), "Classification roles")
  decisions$segment_role <- "TRIBUTARY"
  decisions$decision_notes <- " "
  expect_error(classify_stream_network_segments(f$p, decisions, "fixture"), "nonempty")
  decisions$decision_notes <- "test"
  expect_error(classify_stream_network_segments(f$p, rbind(decisions, decisions), "fixture"), "unique")
  decisions$stream_network_segment_id <- .fg_generate_uuid(1)
  expect_error(classify_stream_network_segments(f$p, decisions, "fixture"), "belong")
  decisions$stream_network_segment_id <- f$p$stream_network$stream_network_segment_id
  f$p$stream_network$review_status <- "ACCEPTED"
  expect_error(classify_stream_network_segments(f$p, decisions, "fixture"), "Reopen")
})

test_that("read-only validation separates readiness from actual acceptance", {
  f <- acceptance_test_fixture()
  initial <- acceptance_test_validate(f)
  expect_setequal(initial$stream_network_validation_issue$issue_code,
                  c("SEGMENT_ROLE_UNRESOLVED", "REQUIRED_REVIEW_PENDING", "OBSERVATION_QUALIFICATION_REQUIRED"))
  f <- acceptance_test_classify(f)
  expect_equal(acceptance_test_validate(f, "WORKING")$stream_network_validation_run$result, "PASS")
  expect_equal(acceptance_test_validate(f)$stream_network_validation_run$result, "REVIEW_REQUIRED")
  f <- acceptance_test_review(f)
  before <- f
  passed <- acceptance_test_validate(f)
  expect_identical(f, before)
  expect_equal(passed$stream_network_validation_run$result, "PASS")
  expect_equal(passed$stream_network_validation_run$validation_level, "ACCEPTANCE")
  expect_equal(nrow(passed$stream_network_validation_issue), 0L)
  expect_equal(f$obs$review_status, "DRAFT")
  expect_equal(f$p$stream_network$review_status, "PENDING")
  expect_false(identical(acceptance_test_validate(f)$stream_network_validation_run$stream_network_validation_run_id,
                         passed$stream_network_validation_run$stream_network_validation_run_id))
})

test_that("current state invalidates stale or forged passed results", {
  good <- acceptance_test_review(acceptance_test_fixture())
  cases <- list(
    role = list(code = "SEGMENT_ROLE_UNRESOLVED", mutate = function(f) {f$p$stream_network$segment_role <- "UNRESOLVED"; f}),
    node = list(code = "NODE_TABLE_MISMATCH", mutate = function(f) {f$p$stream_network_node$in_degree[1] <- 99L; f}),
    missing_node = list(code = "NODE_TABLE_MISMATCH", mutate = function(f) {f$p$stream_network_node <- f$p$stream_network_node[0,]; f}),
    connection = list(code = "CONNECTION_TABLE_MISMATCH", mutate = function(f) {f$p$stream_network_connection <- f$p$stream_network_connection[0,]; f}),
    lineage = list(code = "SOURCE_LINEAGE_INVALID", mutate = function(f) {f$p$stream_network_source$stream_network_segment_id[1] <- .fg_generate_uuid(1); f}),
    operations = list(code = "OPERATION_HISTORY_INVALID", mutate = function(f) {f$p$stream_network_operation$operation_sequence[2] <- 1L; f}),
    stream = list(code = "STREAM_MEMBERSHIP_INVALID", mutate = function(f) {f$p$stream_network$stream_id <- .fg_generate_uuid(1); f}),
    reach = list(code = "REACH_MEMBERSHIP_UNVERIFIED", mutate = function(f) {f$p$stream_network$reach_id <- .fg_generate_uuid(1); f}),
    pending = list(code = "REQUIRED_REVIEW_PENDING", mutate = function(f) {f$p$stream_network_review$decision <- "PENDING"; f}),
    rejected = list(code = "REQUIRED_REVIEW_PENDING", mutate = function(f) {f$p$stream_network_review$decision <- "REJECT"; f}),
    actor = list(code = "REVIEW_PROVENANCE_REQUIRED", mutate = function(f) {f$p$stream_network_review$decision_by <- NA_character_; f}),
    stale = list(code = "REVIEW_STALE", mutate = function(f) {f$p$stream_network_review$decision_at <- Sys.time() - 3600; f}),
    geometry = list(code = "REVIEW_GEOMETRY_STALE", mutate = function(f) {sf::st_geometry(f$p$stream_network_review) <- sf::st_reverse(sf::st_geometry(f$p$stream_network_review)); f}),
    notes = list(code = "OBSERVATION_QUALIFICATION_REQUIRED", mutate = function(f) {f$obs$review_notes <- NA_character_; f}),
    crs = list(code = "CRS_METADATA_MISMATCH", mutate = function(f) {f$obs$native_horizontal_crs <- "EPSG:3857"; f}),
    direction = list(code = "CONNECTIVITY_DIRECTION_UNRESOLVED", mutate = function(f) {f$p$stream_network$direction_status <- "UNRESOLVED"; f})
  )
  for (case in cases) {
    f <- case$mutate(good)
    f$p$stream_network_validation_run$result <- "PASS" # stale history cannot override current data
    result <- acceptance_test_validate(f)
    expect_equal(result$stream_network_validation_run$result, "REVIEW_REQUIRED")
    expect_true(case$code %in% result$stream_network_validation_issue$issue_code, info = case$code)
  }
})

test_that("post-review role edits and role history changes cannot silently pass", {
  f <- acceptance_test_review(acceptance_test_fixture())
  f$p <- classify_stream_network_segments(f$p, data.frame(
    stream_network_segment_id = f$p$stream_network$stream_network_segment_id,
    segment_role = "TRIBUTARY", decision_notes = "Synthetic changed decision"),
    actor = "fixture", performed_at = Sys.time() + 60)
  expect_true("REVIEW_STALE" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
  f$p$stream_network$segment_role <- "ARTIFICIAL"
  expect_true("ROLE_HISTORY_MISMATCH" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
  f$p$stream_network$direction_method <- "LEGACY_UNKNOWN"
  expect_true("DIRECTION_METHOD_REQUIRED" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
})

test_that("matching Reach membership is checked without enterprise lookup", {
  f <- acceptance_test_review(acceptance_test_fixture())
  f$p$stream_network$reach_id <- .fg_generate_uuid(1)
  reaches <- data.frame(reach_id = f$p$stream_network$reach_id, stream_id = f$p$stream_network$stream_id)
  expect_equal(acceptance_test_validate(f, reaches = reaches)$stream_network_validation_run$result, "PASS")
  reaches$stream_id <- .fg_generate_uuid(1)
  expect_true("REACH_MEMBERSHIP_INVALID" %in% acceptance_test_validate(f, reaches = reaches)$stream_network_validation_issue$issue_code)
})

test_that("legacy CRS names and missing or rejected metadata are assessed explicitly", {
  f <- acceptance_test_review(acceptance_test_fixture())
  f$obs$native_horizontal_crs <- sf::st_crs(f$p$stream_network)$Name
  expect_equal(acceptance_test_validate(f)$stream_network_validation_run$result, "PASS")
  f$obs$coverage_status <- NA_character_
  expect_true("OBSERVATION_METADATA_INVALID" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
  f$obs$coverage_status <- "PARTIAL_CONFIGURATION"
  f$obs$review_status <- "REJECTED"
  expect_true("LIFECYCLE_NOT_ELIGIBLE" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
  f$obs$review_status <- "DRAFT"
  f$p$stream_network_operation$operation_code[1] <- "UNKNOWN_REPAIR"
  expect_true("OPERATION_HISTORY_INVALID" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
  f$p$stream_network_operation$operation_code[1] <- "CONSOLIDATE_SEGMENTS"
  f$p$stream_network_operation$operation_notes[1] <- NA_character_
  expect_true("OPERATION_HISTORY_INVALID" %in% acceptance_test_validate(f)$stream_network_validation_issue$issue_code)
})
