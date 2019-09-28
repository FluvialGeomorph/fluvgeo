#' @title Check the validity of an `fgm` `bankline_points` data structure
#'
#' @description Checks that the input data structure `bankline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param bankline_points   SpatialPointsDataFrame; a `bankline_points` data
#'                          structure used by the fgm package.
#'
#' @return Returns TRUE if the `bankline_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_bankline_points <- function(bankline_points) {
  name <- deparse(substitute(bankline_points))

  assert_that(class(bankline_points)[1] == "SpatialPointsDataFrame",
              msg = paste(name, " must be a SpatialPointsDataFrame"))
  assert_that(is.data.frame(bankline_points@data),
              msg = paste(name, " must be a data frame"))
  assert_that("ReachName" %in% colnames(bankline_points@data) &
                is.character(bankline_points@data$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("bank" %in% colnames(bankline_points@data) &
                is.character(bankline_points@data$bank),
              msg = paste("Numeric field 'bank' missing from ", name))
  assert_that("DEM_Z" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$DEM_Z),
              msg = paste("Numeric field 'DEM_Z' missing from ", name))
  assert_that("loop" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$loop),
              msg = paste("Numeric field 'loop' missing from ", name))
  assert_that("bend" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$bend),
              msg = paste("Numeric field 'bend' missing from ", name))
  assert_that("position" %in% colnames(bankline_points@data) &
                is.character(bankline_points@data$position),
              msg = paste("Numeric field 'position' missing from ", name))
  assert_that("bank_POINT_X" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$bank_POINT_X),
              msg = paste("Numeric field 'bank_POINT_X' missing from ", name))
  assert_that("bank_POINT_Y" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$bank_POINT_Y),
              msg = paste("Numeric field 'bank_POINT_Y' missing from ", name))
  assert_that("bank_POINT_M" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$bank_POINT_M),
              msg = paste("Numeric field 'bank_POINT_M' missing from ", name))
  assert_that("valley_POINT_X" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$valley_POINT_X),
              msg = paste("Numeric field 'valley_POINT_X' missing from ", name))
  assert_that("valley_POINT_Y" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$valley_POINT_Y),
              msg = paste("Numeric field 'valley_POINT_Y' missing from ", name))
  assert_that("valley_POINT_M" %in% colnames(bankline_points@data) &
                is.numeric(bankline_points@data$valley_POINT_M),
              msg = paste("Numeric field 'valley_POINT_M' missing from ", name))

  # Check that the `ReachName` field is populated
  assert_that(nchar(unique(bankline_points@data$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check that the `bank` field is populated
  assert_that(all(unique(bankline_points@data$bank) ==
                    c("right descending", "left descending")),
              msg = paste("Field `bank` in", name, "must contain a `right
                          descending` bank and a `left descending` bank."))

  # Check each bankline digitized from the downstream end to the upstream end
  ## Right descending bank
  bp_r <- bankline_points[bankline_points@data$bank == "right descending", ]

  ### Right bank m-value min and max
  r_m_min <- min(bp_r$bank_POINT_M)
  r_m_max <- max(bp_r$bank_POINT_M)

  ### Right bank z-value min and max
  r_m_min_z <- min(bp_r[bp_r$bank_POINT_M == r_m_min, ]$DEM_Z)
  r_m_max_z <- max(bp_r[bp_r$bank_POINT_M == r_m_max, ]$DEM_Z)

  assert_that(r_m_min_z < r_m_max_z,
              msg = paste("Right descending bank in", name,
                          "is not digitized in the upstream direction."))

  ## Left descending bank
  bp_l <- bankline_points[bankline_points@data$bank == "left descending", ]

  ### Left bank m-value min and max
  l_m_min <- min(bp_l$bank_POINT_M)
  l_m_max <- max(bp_l$bank_POINT_M)

  ### Right bank z-value min and max
  l_m_min_z <- min(bp_l[bp_l$bank_POINT_M == l_m_min, ]$DEM_Z)
  l_m_max_z <- max(bp_l[bp_l$bank_POINT_M == l_m_max, ]$DEM_Z)

  assert_that(l_m_min_z < l_m_max_z,
              msg = paste("Left descending bank in", name,
                          "is not digitized in the upstream direction."))

  # Return TRUE if all assertions are met
  TRUE
}

