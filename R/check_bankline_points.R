#' @title Check the validity of an `fgm` `bankline_points` data structure
#'
#' @description Checks that the input data structure `bankline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param bankline_points   data frame; a `bankline_points` data structure used
#'                          by the fgm package.
#'
#' @return Returns TRUE if the `bankline_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_bankline_points <- function(bankline_points) {
  # Check data structure
  name <- deparse(substitute(bankline_points))

  assert_that(is.data.frame(bankline_points),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(bankline_points) &
                is.character(bankline_points$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("bank" %in% colnames(bankline_points) &
                is.character(bankline_points$bank),
              msg = paste("Numeric field 'bank' missing from ", name))
  assert_that("DEM_Z" %in% colnames(bankline_points) &
                is.numeric(bankline_points$DEM_Z),
              msg = paste("Numeric field 'DEM_Z' missing from ", name))
  assert_that("loop" %in% colnames(bankline_points) &
                is.numeric(bankline_points$loop),
              msg = paste("Numeric field 'loop' missing from ", name))
  assert_that("bend" %in% colnames(bankline_points) &
                is.numeric(bankline_points$bend),
              msg = paste("Numeric field 'bend' missing from ", name))
  assert_that("position" %in% colnames(bankline_points) &
                is.character(bankline_points$position),
              msg = paste("Numeric field 'position' missing from ", name))
  assert_that("bank_POINT_X" %in% colnames(bankline_points) &
                is.numeric(bankline_points$bank_POINT_X),
              msg = paste("Numeric field 'bank_POINT_X' missing from ", name))
  assert_that("bank_POINT_Y" %in% colnames(bankline_points) &
                is.numeric(bankline_points$bank_POINT_Y),
              msg = paste("Numeric field 'bank_POINT_Y' missing from ", name))
  assert_that("bank_POINT_M" %in% colnames(bankline_points) &
                is.numeric(bankline_points$bank_POINT_M),
              msg = paste("Numeric field 'bank_POINT_M' missing from ", name))
  assert_that("valley_POINT_X" %in% colnames(bankline_points) &
                is.numeric(bankline_points$valley_POINT_X),
              msg = paste("Numeric field 'valley_POINT_X' missing from ", name))
  assert_that("valley_POINT_Y" %in% colnames(bankline_points) &
                is.numeric(bankline_points$valley_POINT_Y),
              msg = paste("Numeric field 'valley_POINT_Y' missing from ", name))
  assert_that("valley_POINT_M" %in% colnames(bankline_points) &
                is.numeric(bankline_points$valley_POINT_M),
              msg = paste("Numeric field 'valley_POINT_M' missing from ", name))

  # Logical checks
  #Get min and max POINT_M value
  m_min <- min(bankline_points$bank_POINT_M)
  m_max <- max(bankline_points$bank_POINT_M)

  # Calculate min and max z
  m_min_z <- min(bankline_points[bankline_points$bank_POINT_M == m_min, ]$DEM_Z)
  m_max_z <- max(bankline_points[bankline_points$bank_POINT_M == m_max, ]$DEM_Z)

  assert_that(m_min_z < m_max_z,
              msg = paste("feature is not digitized in the upstream direction", name))
}
