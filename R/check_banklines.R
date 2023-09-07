#' @title Check the validity of an `fluvgeo` `banklines` data structure
#'
#' @description Checks that the input data structure `banklines` meets
#' the requirements for this data structure.
#'
#' @export
#' @param banklines   sf object; a `banklines` data structure used
#'                    by the fluvgeo package.
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
  if(class(banklines)[1] == "sf") {
    banklines_df <- banklines
  }

  assert_that((class(banklines)[1] == "sf")  ,
              msg = paste(name, " must be sf"))
  assert_that(is.data.frame(banklines_df),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(banklines_df) &
                is.character(banklines_df$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("bank" %in% colnames(banklines_df) &
                is.character(banklines_df$bank),
              msg = paste("Numeric field 'bank' missing from ", name))
  # assert_that("from_measure" %in% colnames(banklines_df) &
  #               is.numeric(banklines_df$from_measure),
  #             msg = paste("Numeric field 'from_measure' missing from", name))
  # assert_that("to_measure" %in% colnames(banklines_df) &
  #               is.numeric(banklines_df$to_measure),
  #             msg = paste("Numeric field 'to_measure' missing from", name))

  # Check that there are two and only two records
  assert_that(length(unique(banklines_df$bank_id)) == 2,
              msg = paste(name, "must have two and only two records, one for",
                          "the left descending bank and another for the right."))

  # Check that the `bank` field is populated
  assert_that(length(banklines_df[banklines_df$bank ==
                                      "right descending", ]$bank) == 1,
              msg = paste("The `right descending` bank is missing from", name))
  assert_that(length(banklines_df[banklines_df$bank ==
                                      "left descending", ]$bank) == 1,
              msg = paste("The `left descending` bank is missing from", name))

  # Check that each bankline has greater than zero length
  assert_that(all(banklines_df$from_measure < banklines_df$to_measure),
              msg = paste("Banklines", name,
                          "appears to have banks of zero length"))

  # Return TRUE if all assertions are met
  TRUE
}
