#' @title Check the validity of an `fluvgeo` `valleyline_points` data structure
#'
#' @description Checks that the input data structure `valleyline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param valleyline_points   sf: a `valleyline_points`
#'                            data structure used by the fluvgeo package.
#'
#' @return Returns TRUE if the `valleyline_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_valleyline_points <- function(valleyline_points) {
  name <- deparse(substitute(valleyline_points))

  # Check data structure
  assert_that(class(valleyline_points)[1] == "sf",
              msg = paste(name, "must be sf"))
  assert_that(is.data.frame(valleyline_points),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(valleyline_points) &
                is.character(valleyline_points$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("POINT_X" %in% colnames(valleyline_points) &
                is.numeric(valleyline_points$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(valleyline_points) &
                is.numeric(valleyline_points$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(valleyline_points) &
                is.numeric(valleyline_points$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))

  # Check the field `ReachName` is not empty
  assert_that(nchar(unique(valleyline_points$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))
}
