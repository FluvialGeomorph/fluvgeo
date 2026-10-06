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
#'   additionally requires kilometer `POINT_M`/`km_to_mouth` equality and unit
#'   declarations for the ArcPy-replacement chain.
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
  assert_that(all(diff(flowline_points$POINT_M) >= 0),
              msg = paste(name, "POINT_M must be ordered downstream to upstream"))

  if (identical(contract, "fgstudio_replacement")) {
    assert_that("km_to_mouth" %in% names(flowline_points) &&
                  is.numeric(flowline_points$km_to_mouth) &&
                  !anyNA(flowline_points$km_to_mouth) &&
                  all(is.finite(flowline_points$km_to_mouth)),
                msg = paste("Numeric field 'km_to_mouth' missing from", name))
    assert_that(isTRUE(all.equal(flowline_points$POINT_M,
                                flowline_points$km_to_mouth,
                                tolerance = sqrt(.Machine$double.eps))),
                msg = "FG Studio replacement POINT_M and km_to_mouth must be identical kilometres")
    assert_that("POINT_M_units" %in% names(flowline_points) &&
                  all(flowline_points$POINT_M_units == "km") &&
                  "km_to_mouth_units" %in% names(flowline_points) &&
                  all(flowline_points$km_to_mouth_units == "km"),
                msg = "FG Studio replacement measure-unit fields must be 'km'")
  }

  # Check flowline is digitized from downstream end to upstream end
  ## Get min and max POINT_M value
  m_min <- min(flowline_points$POINT_M)
  m_max <- max(flowline_points$POINT_M)

  ## Calculate min and max z
  m_min_z <- min(flowline_points[flowline_points$POINT_M == m_min, ]$Z)
  m_max_z <- max(flowline_points[flowline_points$POINT_M == m_max, ]$Z)

  ## Check downstream end is a lower elevation than upstream end
  assert_that(m_min_z < m_max_z,
              msg = paste("The flowline used to create", name,
                          "is not digitized beginning at the downstream end."))

  TRUE
}
