#' @title Check the validity of an `fgm` `banklines` data structure
#'
#' @description Checks that the input data structure `banklines` meets
#' the requirements for this data structure.
#'
#' @export
#' @param banklines   SpatialLinesDataFrame; a `banklines` data structure used
#'                    by the fgm package.
#'
#' @return Returns TRUE if the `banklines` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_banklines <- function(banklines) {
  name <- deparse(substitute(banklines))

  assert_that(class(banklines)[1] == "SpatialLinesDataFrame",
              msg = paste(name, " must be a SpatialLinesDataFrame"))
  assert_that(is.data.frame(banklines@data),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(banklines@data) &
                is.character(banklines@data$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("bank" %in% colnames(banklines@data) &
                is.character(banklines@data$bank),
              msg = paste("Numeric field 'bank' missing from ", name))
  assert_that("from_measure" %in% colnames(banklines@data) &
                is.numeric(banklines@data$from_measure),
              msg = paste("Numeric field 'from_measure' missing from", name))
  assert_that("to_measure" %in% colnames(banklines@data) &
                is.numeric(banklines@data$to_measure),
              msg = paste("Numeric field 'to_measure' missing from", name))

  # Check that there are two and only two records
  assert_that(length(unique(banklines@data$OBJECTID)) == 2,
              msg = paste(name, "must have two and only two records, one for",
                          "the left descending bank and another for the right."))

  # Check that the `bank` field is populated
  assert_that(length(banklines@data[banklines@data$bank ==
                                      "right descending", ]$bank) == 1,
              msg = paste("The `right descending` bank is missing from", name))
  assert_that(length(banklines@data[banklines@data$bank ==
                                      "left descending", ]$bank) == 1,
              msg = paste("The `left descending` bank is missing from", name))

  # Check that each bankline has greater than zero length
  assert_that(all(banklines@data$from_measure < banklines@data$to_measure),
              msg = paste("Banklines", name,
                          "appears to have banks of zero length"))

  # Return TRUE if all assertions are met
  TRUE
}
