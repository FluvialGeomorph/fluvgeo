#' @title Check the validity of an `fluvgeo` `flowline_points` data structure
#'
#' @description Checks that the input data structure `flowline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param flowline_points sf object; a `flowline_points` data structure used by
#'   the fluvgeo package.
#' @param contract character; `"legacy_core"` validates the historical common
#'   field contract without assuming measure units. `"fgstudio_replacement"`
#'   additionally requires kilometer `POINT_M`, `POINT_M_uncalibrated`,
#'   `calibration_diff`, and `km_to_mouth` relationships and unit declarations
#'   for the ArcPy-replacement chain.
#'
#' @return Returns TRUE if the `flowline_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_flowline_points <- function(flowline_points,
                                  contract = c("legacy_core",
                                               "fgstudio_replacement")) {
  contract <- match.arg(contract)
  name <- deparse(substitute(flowline_points))

  # Check data structure
  assert_that(inherits(flowline_points, "sf"),
              msg = paste(name, "must be a sf object"))
  assert_that(is.data.frame(flowline_points),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline_points) &
                is.character(flowline_points$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("POINT_X" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))
  assert_that("Z" %in% colnames(flowline_points) &
                is.numeric(flowline_points$Z),
              msg = paste("Numeric field 'Z' missing from ", name))
  assert_that(!is.na(sf::st_crs(flowline_points)),
              msg = paste(name, "must have a defined CRS"))
  xy_geometry <- sf::st_zm(flowline_points, drop = TRUE, what = "ZM")
  assert_that(all(as.character(sf::st_geometry_type(flowline_points)) == "POINT") &&
                !any(sf::st_is_empty(flowline_points)) &&
                all(sf::st_is_valid(xy_geometry)),
              msg = paste(name, "must contain valid, nonempty point geometry"))

  # Check the field `ReachName` is not empty
  assert_that(!anyNA(flowline_points$ReachName) &&
                all(nzchar(trimws(flowline_points$ReachName))),
              msg = paste("Field `ReachName` is empty in", name))

  numeric_fields <- c("POINT_X", "POINT_Y", "POINT_M", "Z")
  numeric_values <- sf::st_drop_geometry(flowline_points)[numeric_fields]
  assert_that(all(vapply(numeric_values, function(x) {
    !anyNA(x) && all(is.finite(x))
  }, logical(1))), msg = paste(name, "contains missing or non-finite coordinates, measures, or elevations"))
  coordinates <- sf::st_coordinates(flowline_points)
  coordinate_tolerance <- sqrt(.Machine$double.eps) *
    max(1, abs(c(coordinates[, c("X", "Y")], flowline_points$POINT_X,
      flowline_points$POINT_Y)))
  assert_that(all(abs(coordinates[, "X"] - flowline_points$POINT_X) <= coordinate_tolerance) &&
                all(abs(coordinates[, "Y"] - flowline_points$POINT_Y) <= coordinate_tolerance),
              msg = paste(name, "POINT_X/POINT_Y do not match point geometry"))
  measure_tolerance <- sqrt(.Machine$double.eps) *
    max(1, abs(flowline_points$POINT_M))
  measure_groups <- if ("stream_id" %in% names(flowline_points))
    split(flowline_points$POINT_M, flowline_points$stream_id) else
    list(flowline_points$POINT_M)
  assert_that(all(vapply(measure_groups, function(x)
    all(diff(x) >= -measure_tolerance), logical(1))),
    msg = paste(name, "POINT_M must be ordered downstream to upstream within each Stream"))

  if (identical(contract, "fgstudio_replacement")) {
    replacement_fields <- c("POINT_M_uncalibrated", "calibration_diff",
                            "km_to_mouth")
    assert_that(all(replacement_fields %in% names(flowline_points)) &&
                  all(vapply(sf::st_drop_geometry(flowline_points)[replacement_fields],
                    function(x) is.numeric(x) && !anyNA(x) && all(is.finite(x)),
                    logical(1))),
                msg = paste(name, "must contain finite numeric replacement measure fields"))
    assert_that(isTRUE(all.equal(flowline_points$POINT_M,
                                flowline_points$km_to_mouth,
                                tolerance = sqrt(.Machine$double.eps))),
                msg = "FG Studio replacement POINT_M and km_to_mouth must be identical kilometres")
    assert_that(isTRUE(all.equal(
                  flowline_points$POINT_M - flowline_points$POINT_M_uncalibrated,
                  flowline_points$calibration_diff,
                  tolerance = sqrt(.Machine$double.eps))),
                msg = "calibration_diff must equal POINT_M minus POINT_M_uncalibrated")
    assert_that("POINT_M_units" %in% names(flowline_points) &&
                  all(flowline_points$POINT_M_units == "km") &&
                  "POINT_M_uncalibrated_units" %in% names(flowline_points) &&
                  all(flowline_points$POINT_M_uncalibrated_units == "km") &&
                  "calibration_diff_units" %in% names(flowline_points) &&
                  all(flowline_points$calibration_diff_units == "km") &&
                  "km_to_mouth_units" %in% names(flowline_points) &&
                  all(flowline_points$km_to_mouth_units == "km"),
                msg = "FG Studio replacement measure-unit fields must be 'km'")
    if ("reference_frame_scope" %in% names(flowline_points) &&
        any(flowline_points$reference_frame_scope == "STUDY_AREA_NETWORK")) {
      network_fields <- c("stream_id", "stream_name", "downstream_stream_id",
        "confluence_measure_km", "stream_offset_km", "measure_origin")
      assert_that(all(network_fields %in% names(flowline_points)),
        msg = "Study Area network points must contain Stream and confluence fields")
      assert_that(!anyNA(flowline_points[c("stream_id", "stream_name",
          "confluence_measure_km", "stream_offset_km", "measure_origin")]) &&
          all(flowline_points$measure_origin == "STUDY_AREA_OUTLET") &&
          all(flowline_points$reference_frame_scope == "STUDY_AREA_NETWORK"),
        msg = "Study Area network points must use one declared Study Area outlet frame")
      stream_offsets <- vapply(split(seq_len(nrow(flowline_points)),
        flowline_points$stream_id), function(i) {
          values <- unique(flowline_points$stream_offset_km[i])
          if (length(values) != 1L) return(NA_real_)
          values
        }, numeric(1))
      assert_that(!anyNA(stream_offsets) && sum(abs(stream_offsets) <=
          measure_tolerance) == 1L,
        msg = "Study Area network points must have exactly one outlet Stream at zero")
      assert_that(all(abs(flowline_points$stream_offset_km -
          flowline_points$confluence_measure_km) <= measure_tolerance) &&
          all(flowline_points$POINT_M >= flowline_points$stream_offset_km -
            measure_tolerance),
        msg = "Study Area network Stream measures must begin at their confluence measure")
    }
  }

  # Check flowline is digitized from downstream end to upstream end
  direction_groups <- if ("stream_id" %in% names(flowline_points))
    split(seq_len(nrow(flowline_points)), flowline_points$stream_id) else
    list(seq_len(nrow(flowline_points)))
  direction_ok <- vapply(direction_groups, function(i) {
    values <- flowline_points$POINT_M[i]
    downstream <- flowline_points$Z[i][values == min(values)]
    upstream <- flowline_points$Z[i][values == max(values)]
    min(downstream) < max(upstream)
  }, logical(1))
  assert_that(all(direction_ok),
    msg = paste("A flowline used to create", name,
      "is not digitized beginning at the downstream end."))

  TRUE
}
