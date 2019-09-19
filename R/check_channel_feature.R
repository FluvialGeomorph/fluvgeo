#' @title Check the validity of an `fgm` `channel_feature` data structure
#'
#' @description Checks that the input data structure `channel_feature` meets
#' the requirements for this data structure.
#'
#' @export
#' @param channel_feature   data frame; a `channel_feature` data structure used
#'                          by the fgm package.
#'
#' @return Returns TRUE if the `channel_feature` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
check_channel_feature <- function(channel_feature) {
  name <- deparse(substitute(channel_feature))

  assert_that(is.data.frame(channel_feature),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(channel_feature) &
                is.character(channel_feature$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("POINT_X" %in% colnames(channel_feature) &
                is.numeric(channel_feature$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(channel_feature) &
                is.numeric(channel_feature$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(channel_feature) &
                is.numeric(channel_feature$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))
}
