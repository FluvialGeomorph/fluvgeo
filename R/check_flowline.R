#' @title Check the validity of an `fluvgeo` `flowline` data structure
#'
#' @description Checks that the input data structure `flowline` meets
#' the required legacy field, line geometry, CRS, cardinality and stationing
#' requirements for the selected processing step.
#'
#' @export
#' @param flowline        sf: a `flowline` data structure
#'                        used by the fluvgeo package.
#' @param step            character; last completed processing step. One of
#'                        "create_flowline", "profile_points"
#'
#' @return Returns TRUE if the `flowline` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_flowline <- function(flowline,
                           step = c("create_flowline", "profile_points")) {
  step <- match.arg(step)
  name <- deparse(substitute(flowline))
  assert_that(inherits(flowline, "sf"),
              msg = paste(name, "must be a sf object"))
  flowline_df <- flowline

  # Step: create_flowline
  if(step %in% c("create_flowline", "profile_points")) {
  assert_that(is.data.frame(flowline_df),
              msg = paste(name, "must be a data frame"))
  assert_that(!is.na(sf::st_crs(flowline_df)),
              msg = paste(name, "must have a defined CRS"))
  geometry_type <- unique(as.character(sf::st_geometry_type(flowline_df)))
  assert_that(length(geometry_type) == 1L &&
                geometry_type %in% c("LINESTRING", "MULTILINESTRING") &&
                !any(sf::st_is_empty(flowline_df)) &&
                all(sf::st_is_valid(flowline_df)),
              msg = paste(name, "must contain valid, nonempty line geometry"))
  assert_that("ReachName" %in% colnames(flowline_df) &
                is.character(flowline_df$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))

  # Check the field `ReachName` is not empty
  assert_that(!anyNA(flowline_df$ReachName) &&
                all(nzchar(trimws(flowline_df$ReachName))),
              msg = paste("Field `ReachName` is empty in", name))

  # Check that there is only one flowline
  assert_that(length(flowline_df$ReachName) == 1,
              msg = paste("Flowline", name, "can only have one record"))
  }

  # Step: profile_points
  if(step %in% c("profile_points")) {
  assert_that("from_measure" %in% colnames(flowline_df) &
                is.numeric(flowline_df$from_measure),
              msg = paste("Numeric field 'from_measure' missing from", name))
  assert_that("to_measure" %in% colnames(flowline_df) &
                is.numeric(flowline_df$to_measure),
              msg = paste("Numeric field 'to_measure' missing from", name))

  # Check that flowline has greater than zero length
  assert_that(!anyNA(flowline_df$from_measure) &&
                !anyNA(flowline_df$to_measure) &&
                all(is.finite(flowline_df$from_measure)) &&
                all(is.finite(flowline_df$to_measure)) &&
                all(flowline_df$from_measure >= 0) &&
                all(flowline_df$from_measure < flowline_df$to_measure),
              msg = paste("The flowline", name, "appears to have zero length"))
  }

  # Return TRUE if all assertions are met
  TRUE
}
