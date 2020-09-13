#' @title Check the validity of an `fluvgeo` `cross_section` data structure
#'
#' @description Checks that the input data structure `cross_section` meets
#' the requirements for this data structure.
#'
#' @export
#' @param cross_section   SpatialLinesDataFrame of sf: a `cross_section` data
#'                        structure used by the fluvgeo package.
#' @param step            character; last completed processing step. One of
#'                        "assign_ids", "watershed_area", "river_position",
#'                        "station_points", "loop_bend"
#'
#' @details Cross section feature classes evolve as different steps are
#' performed on them. The `step` parameter allows a `cross section` data
#' structure to be checked throughout its lifecycle. Each step defines a
#' changing set of requirements for the `cross section` data structure.
#'
#' @return Returns TRUE if the `cross_section` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_cross_section <- function(cross_section,
                                step = c("assign_ids", "watershed_area",
                                         "river_position", "station_points",
                                         "loop_bend")) {
  name <- deparse(substitute(cross_section))

  if(class(cross_section)[1] == "SpatialLinesDataFrame") {
    cross_section_df <- cross_section@data
  }
  if(class(cross_section)[1] == "sf") {
    cross_section_df <- cross_section
  }

  # Step: assign_ids
  if(step %in% c("assign_ids", "watershed_area", "river_position",
             "station_points", "loop_bend")) {
    assert_that((class(cross_section)[1] == "SpatialLinesDataFrame") |
                (class(cross_section)[1] == "sf"),
                msg = paste(name, "must be a SpatialLinesDataFrame of sf"))
    assert_that(is.data.frame(cross_section_df),
                msg = paste(name, "must be a data frame"))
    assert_that("ReachName" %in% colnames(cross_section_df) &
                  is.character(cross_section_df$ReachName),
                msg = paste("Character field 'ReachName' missing from", name))
    assert_that("Seq" %in% colnames(cross_section_df) &
                  is.numeric(cross_section_df$Seq),
                msg = paste("Numeric field 'Seq' missing from", name))

    # Check the field `ReachName` is not empty
    assert_that(nchar(unique(cross_section_df$ReachName)[1]) > 0,
                msg = paste("Field `ReachName` is empty in", name))

    # Check for duplicate or missing `Seq` values
    assert_that(length(unique(cross_section_df$Seq)) ==
                  length(min(cross_section_df$Seq):max(cross_section_df$Seq)),
                msg = paste("Check for duplicate or missing `Seq` values in", name))

  }
  # Step: watershed_area
  if(step %in% c("watershed_area", "river_position", "station_points",
                 "loop_bend")) {
    assert_that("Watershed_Area_SqMile" %in% colnames(cross_section_df) &
                  is.numeric(cross_section_df$Watershed_Area_SqMile),
                msg = paste("Numeric field 'Watershed_Area_SqMile' missing from",
                            name))

    # Check that all cross sections have watershed areas
    assert_that((sum(is.na(cross_section_df$Watershed_Area_SqMi)) +
                   sum(na.omit(cross_section_df$Watershed_Area_SqMile) == 0)) == 0,
                msg = paste("Field `Watershed_Area_SqMile` contains missing values",
                            name))
  }

  # Step: river_position
  if(step %in% c("river_position", "station_points", "loop_bend")) {
    assert_that("POINT_X" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$POINT_X),
                msg = paste("Numeric field 'POINT_X' missing from", name))
    assert_that("POINT_Y" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$POINT_Y),
                msg = paste("Numeric field 'POINT_Y' missing from", name))
    assert_that("POINT_M" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$POINT_M),
                msg = paste("Numeric field 'POINT_M' missing from", name))
    assert_that("Z" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$Z),
                msg = paste("Numeric field 'Z' missing from", name))
    assert_that("km_to_mouth" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$km_to_mouth),
                msg = paste("Numeric field 'km_to_mouth' missing from", name))

    # Check that cross sections are numbered beginning at the downstream end
    min_seq <- min(cross_section_df$Seq)
    max_seq <- max(cross_section_df$Seq)

    xs_1 <- cross_section_df[cross_section_df$Seq == min_seq, ]
    min_seq_elevation <- min(xs_1$Z)

    xs_n <- cross_section_df[cross_section_df$Seq == max_seq, ]
    max_seq_elevation <- min(xs_n$Z)

    assert_that(min_seq_elevation < max_seq_elevation,
                msg = paste("Cross section numbering doesn't begin at the ",
                            "downstream end of the reach in ", name))
  }

  # Step: station_points
  if(step %in% c("station_points", "loop_bend")) {
    assert_that("from_measure" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$from_measure),
                msg = paste("Numeric field 'from_measure' missing from", name))
    assert_that("to_measure" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$to_measure),
                msg = paste("Numeric field 'to_measure' missing from", name))
  }

  # Step: loop_bend
  if(step %in% c("loop_bend")) {
  assert_that("loop" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$loop),
              msg = paste("Numeric field 'loop' missing from", name))
  assert_that("bend" %in% colnames(cross_section_df) &
                is.numeric(cross_section_df$bend),
              msg = paste("Numeric field 'bend' missing from", name))
  }

  # Return TRUE if all assertions are met
  TRUE
}

