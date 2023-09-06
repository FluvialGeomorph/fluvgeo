#' @title Check the validity of an `fluvgeo` `flowline_points` data structure
#'
#' @description Checks that the input data structure `flowline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param flowline_points   sf object; a `flowline_points` data
#'                          structure used by the fluvgeo package.
#'
#' @return Returns TRUE if the `flowline_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_flowline_points <- function(flowline_points) {
  name <- deparse(substitute(flowline_points))

  # Check data structure
  assert_that(class(flowline_points)[1] == "sf",
              msg = paste(name, "must be a SpatialPointsDataFrame"))
  assert_that(is.data.frame(flowline_points),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline_points) &
                is.character(flowline_points$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("POINT_X" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(flowline_points) &
                is.numeric(flowline_points$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))
  assert_that("Z" %in% colnames(flowline_points) &
                is.numeric(flowline_points$Z),
              msg = paste("Numeric field 'Z' missing from ", name))

  # Check the field `ReachName` is not empty
  assert_that(nchar(unique(flowline_points$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check flowline is digitized from downstream end to upstream end
  ## Get min and max POINT_M value
  m_min <- min(flowline_points$POINT_M)
  m_max <- max(flowline_points$POINT_M)

  ## Calculate min and max z
  m_min_z <- min(flowline_points[flowline_points$POINT_M == m_min, ]$Z)
  m_max_z <- max(flowline_points[flowline_points$POINT_M == m_max, ]$Z)

  ## Check downstream end is a lower elevation than upstream end
  assert_that(m_min_z < m_max_z,
              msg = paste("The flowline used to create", name,
                          "is not digitized beginning at the downstream end."))
}
