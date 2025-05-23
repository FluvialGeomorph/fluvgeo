#' @title Check the validity of an `fluvgeo` `valleyline` data structure
#'
#' @description Checks that the input data structure `valleyline` meets
#' the requirements for this data structure.
#'
#' @export
#' @param valleyline   sf object: a `valleyline` data structure
#'                     used by the fluvgeo package.
#'
#' @return Returns TRUE if the `valleyline` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_valleyline <- function(valleyline) {
  name <- deparse(substitute(valleyline))

  # Check data structure
  assert_that(class(valleyline)[1] == "sf",
              msg = paste(name, " must be a sf object"))
  assert_that(is.data.frame(valleyline),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(valleyline) &
                is.character(valleyline$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("from_measure" %in% colnames(valleyline) &
                is.numeric(valleyline$from_measure),
              msg = paste("Numeric field 'from_measure' missing from", name))
  assert_that("to_measure" %in% colnames(valleyline) &
                is.numeric(valleyline$to_measure),
              msg = paste("Numeric field 'to_measure' missing from", name))

  # Check the field `ReachName` is not empty
  assert_that(nchar(unique(valleyline$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check that the valleyline has greater than zero length
  assert_that(all(valleyline$from_measure < valleyline$to_measure),
              msg = paste("Valleyline", name,
                          "appears to be zero length"))

  # Return TRUE if all assertions are met
  TRUE
}
