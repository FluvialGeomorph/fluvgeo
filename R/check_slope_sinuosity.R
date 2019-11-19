#' @title Check the validity of the `slope_sinuosity` function
#'
#' @description Checks that the output of the `slope_sinuosity` function is
#' valid.
#'
#' @export
#' @param slope_sinuosity_df data frame; a data frame produced by the
#'                           `slope_sinousity` functionv.
#'
#' @return Returns TRUE if the `slope_sinuosity_df` data frame matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_slope_sinuosity <- function(slope_sinuosity_df) {
  name <- deparse(substitute(slope_sinuosity_df))

  assert_that(is.data.frame(slope_sinuosity_df),
              msg = paste(name, " must be a data frame"))
  assert_that("sinuosity" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$sinuosity),
              msg = paste("Character field 'sinuosity' missing from", name))
  assert_that("slope" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$slope),
              msg = paste("Numeric field 'slope' missing from ", name))

  # Check that mean sinuosity is >= 1
  assert_that(mean(slope_sinuosity_df$sinuosity, na.rm = TRUE) >= 1,
              msg = "mean sinuosity is not >= 1")

  # Check that mean sinuosity is < 50 (an arbitrarily large sinuosity)
  assert_that(mean(slope_sinuosity_df$sinuosity, na.rm = TRUE) < 50,
              msg = "mean sinuosity is not < 50")

  # Check slope that mean slope is >= 0
  assert_that(mean(slope_sinuosity_df$slope, na.rm = TRUE) >= 0,
              msg = "mean slope is not >= 0")

  # Return TRUE if all assertions are met
  TRUE
}
