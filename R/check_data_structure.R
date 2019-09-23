#' @title Check the validity of an fgm data structure
#'
#' @description Checks that the input data structure meets the requirements for
#' that data structure.
#'
#' @export
#' @param data_structure   data frame; a data frame used by the fgm package
#' @param data_type        character; the types of data structures used by the
#'                         fgm package.
#'
#' @return Returns TRUE if the `data_structure` parameter matches the
#' requirements. The function throws an error for data structures not matching
#' the data type specification. Returns the string "Not an fgm package data
#' structure" if the `data_type` parameter is not an fgm package data type.
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom assertthat assert_that
#'
check_data_structure <-function(data_structure,
                                data_type = c("bankline_points",
                                              "channel_feature",
                                              "cross_section",
                                              "downhill",
                                              "flowline",
                                              "flowline_points",
                                              "slope_sinuosity")) {

  switch(data_type,
         bankline_points = check_bankline_points(data_structure),
         channel_feature = check_channel_feature(data_structure),
         cross_section   = cross_section_test(data_structure),
         downhill        = check_downhill(data_structure),
         flowline        = flowline_test(data_structure),
         flowline_points = flowline_points_test(data_structure),
         slope_sinuosity = slope_sinuosity_test(data_structure),
         stop("data_structure is not an fgm data structure"))
}




flowline_test <- function(flowline) {
  name <- deparse(substitute(flowline))

  assert_that(class(flowline) == "SpatialLinesDataFrame")
  assert_that(is.data.frame(flowline@data),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline@data) &
                is.character(flowline@data$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("from_measure" %in% colnames(flowline@data) &
                is.numeric(flowline@data$from_measure),
              msg = paste("Numeric field 'from_measure' missing from ", name))
  assert_that("to_measure" %in% colnames(flowline@data) &
                is.numeric(flowline@data$to_measure),
              msg = paste("Numeric field 'to_measure' missing from ", name))
}

flowline_points_test <- function(flowline_points) {
  name <- deparse(substitute(flowline_points))

  assert_that(class(flowline_points) == "SpatialPointsDataFrame")
  assert_that(is.data.frame(flowline_points@data),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(flowline_points@data) &
                is.character(flowline_points@data$ReachName),
              msg = paste("Character field 'ReachName' missing from ", name))
  assert_that("POINT_X" %in% colnames(flowline_points@data) &
                is.numeric(flowline_points@data$POINT_X),
              msg = paste("Numeric field 'POINT_X' missing from ", name))
  assert_that("POINT_Y" %in% colnames(flowline_points@data) &
                is.numeric(flowline_points@data$POINT_Y),
              msg = paste("Numeric field 'POINT_Y' missing from ", name))
  assert_that("POINT_M" %in% colnames(flowline_points@data) &
                is.numeric(flowline_points@data$POINT_M),
              msg = paste("Numeric field 'POINT_M' missing from ", name))
  assert_that("Z" %in% colnames(flowline_points@data) &
                is.numeric(flowline_points@data$Z),
              msg = paste("Numeric field 'Z' missing from ", name))
}


slope_sinuosity_test <- function(slope_sinuosity_df) {
  name <- deparse(substitute(slope_sinuosity_df))

  assert_that(is.data.frame(slope_sinuosity_df),
              msg = paste(name, " must be a data frame"))
  assert_that("upstream_x" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_x),
              msg = paste("Numeric field 'upstream_x' missing from ", name))
  assert_that("upstream_y" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_y),
              msg = paste("Numeric field 'upstream_y' missing from ", name))
  assert_that("downstream_x" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_x),
              msg = paste("Numeric field 'downstream_x' missing from ", name))
  assert_that("downstream_y" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_y),
              msg = paste("Numeric field 'downstream_y' missing from ", name))
  assert_that("upstream_z" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_z),
              msg = paste("Numeric field 'upstream_z' missing from ", name))
  assert_that("downstream_z" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_z),
              msg = paste("Numeric field 'downstream_z' missing from ", name))
  assert_that("upstream_m" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$upstream_m),
              msg = paste("Numeric field 'upstream_m' missing from ", name))
  assert_that("downstream_m" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$downstream_m),
              msg = paste("Numeric field 'downstream_m' missing from ", name))
  assert_that("rise" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$rise),
              msg = paste("Numeric field 'rise' missing from ", name))
  assert_that("run" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$run),
              msg = paste("Numeric field 'run' missing from ", name))
  assert_that("stream_length" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$stream_length),
              msg = paste("Numeric field 'stream_length' missing from ", name))
  assert_that("valley_length" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$valley_length),
              msg = paste("Numeric field 'valley_length' missing from ", name))
  assert_that("sinuosity" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$sinuosity),
              msg = paste("Numeric field 'sinuosity' missing from ", name))
  assert_that("slope" %in% colnames(slope_sinuosity_df) &
                is.numeric(slope_sinuosity_df$slope),
              msg = paste("Numeric field 'slope' missing from ", name))
}




