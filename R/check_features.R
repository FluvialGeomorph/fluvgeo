#' @title Check the validity of an `fgm` `features` data structure
#'
#' @description Checks that the input data structure `features` meets
#' the requirements for this data structure.
#'
#' @export
#' @param features   SpatialPointsDataFrame; a `features` data structure used
#'                   by the fgm package.
#'
#' @return Returns TRUE if the `features` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_features <- function(features) {
  name <- deparse(substitute(features))

  assert_that(class(features)[1] == "SpatialPointsDataFrame",
              msg = paste(name, " must be a SpatialPointsDataFrame"))
  assert_that(is.data.frame(features@data),
              msg = paste(name, " must be a data frame"))
  assert_that("Name" %in% colnames(features@data) &
                is.character(features@data$Name),
              msg = paste("Character field 'Name' missing from", name))
  assert_that("km_to_mouth" %in% colnames(features@data) &
                is.numeric(features@data$km_to_mouth),
              msg = paste("Numeric field 'km_to_mouth' missing from", name))

  # Check the field `Name` is not empty
  assert_that(nchar(unique(features@data$Name[1])) > 0,
              msg = paste("Field `Name` is empty in", name))

  # Check that all `km_to_mouth` values are greater than zero
  assert_that(all(features@data$km_to_mouth > 0),
              msg = paste("Confirm that all features in", name,
                          "have a value for the `km_to_mouth` field. "))

  # Return TRUE if all assertions are met
  TRUE
}
