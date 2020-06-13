#' @title Check the validity of an `fluvgeo` `flowline` data structure
#'
#' @description Checks that the input data structure `flowline` meets
#' the requirements for this data structure.
#'
#' @export
#' @param flowline        SpatialLinesDataFrame: a `flowline` data structure
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

  name <- deparse(substitute(flowline))

  # Step: create_flowline
  if(step %in% c("create_flowline", "profile_points")) {
  assert_that(class(flowline)[1] == "SpatialLinesDataFrame",
              msg = paste(name, "must be a SpatialLinesDataFrame"))
  assert_that(is.data.frame(flowline@data),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline@data) &
                is.character(flowline@data$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))

  # Check the field `ReachName` is not empty
  assert_that(nchar(unique(flowline@data$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check that there is only one flowline
  assert_that(length(flowline@data$ReachName) == 1,
              msg = paste("Flowline", name, "can only have one record"))
  }

  # Step: profile_points
  if(step %in% c("profile_points")) {
  assert_that("from_measure" %in% colnames(flowline@data) &
                is.numeric(flowline@data$from_measure),
              msg = paste("Numeric field 'from_measure' missing from", name))
  assert_that("to_measure" %in% colnames(flowline@data) &
                is.numeric(flowline@data$to_measure),
              msg = paste("Numeric field 'to_measure' missing from", name))

  # Check that flowline has greater than zero length
  assert_that(flowline@data$from_measure < flowline@data$to_measure,
              msg = paste("The flowline", name, "appears to have zero length"))
  }

  # Return TRUE if all assertions are met
  TRUE
}
