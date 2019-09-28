#' @title Check the validity of an `fgm` `cross_section_points` data structure
#'
#' @description Checks that the input data structure `cross_section_points`
#' meets the requirements for this data structure.
#'
#' @export
#' @param cross_section_points   SpatialPointsDataFrame; a `cross_section_points`
#'                               data structure used by the fgm package.
#' @param step            character; last completed processing step. One of
#'                        "station_points", "loop_bend"
#'
#' @return Returns TRUE if the `cross_section_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_cross_section_points <- function(xs_points,
                                       step = c("station_points", "loop_bend")) {

  name <- deparse(substitute(xs_points))

  # Step: station_points
  if(step %in% c("station_points", "loop_bend")) {
    assert_that(class(xs_points)[1] == "SpatialPointsDataFrame",
                msg = paste(name, " must be a SpatialPointsDataFrame"))
    assert_that(is.data.frame(xs_points@data),
                msg = paste(name, " must be a data frame"))
    assert_that("Seq" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$Seq),
                msg = paste("Numeric field 'Seq' missing from ", name))
    assert_that("POINT_X" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$POINT_X),
                msg = paste("Numeric field 'POINT_X' missing from ", name))
    assert_that("POINT_Y" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$POINT_Y),
                msg = paste("Numeric field 'POINT_Y' missing from ", name))
    assert_that("POINT_M" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$POINT_M),
                msg = paste("Numeric field 'POINT_M' missing from ", name))
    assert_that("Watershed_Area_SqMile" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$Watershed_Area_SqMile),
                msg = paste("Numeric field 'Watershed_Area_SqMile' missing from ",
                            name))
    assert_that("ReachName" %in% colnames(xs_points@data) &
                  is.character(xs_points@data$ReachName),
                msg = paste("Character field 'ReachName' missing from", name))
    assert_that("km_to_mouth" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$km_to_mouth),
                msg = paste("Numeric field 'km_to_mouth' missing from ", name))
    assert_that("DEM_Z" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$DEM_Z),
                msg = paste("Numeric field 'DEM_Z' missing from ", name))
    assert_that("Detrend_DEM_Z" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$Detrend_DEM_Z),
                msg = paste("Numeric field 'Detrend_DEM_Z' missing from ", name))

    # Check the `ReachName` field is not empty
    assert_that(nchar(unique(xs_points@data$ReachName[1])) > 0,
                msg = paste("Field `ReachName` is empty in", name))

    # Check cross sections numbered in upstream direction
    min_seq <- min(xs_points@data$Seq)
    max_seq <- max(xs_points@data$Seq)

    xs_1 <- xs_points@data[xs_points@data$Seq == min_seq, ]
    min_seq_elevation <- min(xs_1$DEM_Z)

    xs_n <- xs_points@data[xs_points@data$Seq == max_seq, ]
    max_seq_elevation <- min(xs_n$DEM_Z)

    assert_that(min_seq_elevation < max_seq_elevation,
                msg = paste("Cross section numbering doesn't begin at the ",
                            "downstream end of the reach in ", name))
  }
  # Step: loop_bend
  if(step %in% c("loop_bend")) {
    assert_that("loop" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$loop),
                msg = paste("Numeric field 'loop' missing from ", name))
    assert_that("bend" %in% colnames(xs_points@data) &
                  is.numeric(xs_points@data$bend),
                msg = paste("Numeric field 'bend' missing from ", name))
  }

  # Return TRUE if all assertions are met
  TRUE
}
