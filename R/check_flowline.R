#' @title Check the validity of an `fgm` `flowline` data structure
#'
#' @description Checks that the input data structure `flowline` meets
#' the requirements for this data structure.
#'
#' @export
#' @param flowline          data frame; a `flowline` data structure used
#'                          by the fgm package.
#'
#' @return Returns TRUE if the `flowline` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_flowline <- function(flowline) {
  name <- deparse(substitute(flowline))

  # Check data structure
  assert_that(is.data.frame(flowline),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline) &
                is.character(flowline$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("from_measure" %in% colnames(flowline) &
                is.numeric(flowline$from_measure),
              msg = paste("Numeric field 'from_measure' missing from ", name))
  assert_that("to_measure" %in% colnames(flowline) &
                is.numeric(flowline$to_measure),
              msg = paste("Numeric field 'to_measure' missing from ", name))
}
