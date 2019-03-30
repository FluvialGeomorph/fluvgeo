#' @title Check the validity of an fgm data structure
#'
#' @description Checks that the input data structure meets the requirements for
#' that data structure.
#'
#' @export
#' @param data_structure   data frame; a data frame used by the fgm package
#' @param data_type        character; the types of data structures used by the
#'                         fgm package. One of: 'channel_feature',
#'                         'bankline_points'
#'
#' @return Returns TRUE if the `data_structure` parameter matches the
#' requirements. The function throws an error for data structures not matching
#' the data type specification. Returns the string "Not a listed data structure"
#' if the `data_type` parameter is not a listed package data type.
#'
#' @importFrom assertthat assert_that
#'
check_data_structure <-function(data_structure, data_type) {
  switch(data_type,
         channel_feature = channel_feature_test(data_structure),
         bankline_points = bankline_points_test(data_structure),
         stop("Not a listed data structure"))
}

channel_feature_test <- function(channel_feature) {
  assert_that(is.data.frame(channel_feature),
              msg = "'channel_feature' must be a data frame")
  assert_that("ReachName" %in% colnames(channel_feature),
              msg = "Required field 'ReachName' missing from channel_feature")
  assert_that("POINT_X" %in% colnames(channel_feature),
              msg = "Required field 'POINT_X' missing from channel_feature")
  assert_that("POINT_Y" %in% colnames(channel_feature),
              msg = "Required field 'POINT_Y' missing from channel_feature")
  assert_that("POINT_M" %in% colnames(channel_feature),
              msg = "Required field 'POINT_M' missing from channel_feature")
  assert_that(is.numeric(channel_feature$POINT_X),
              msg = "'POINT_X' must be numeric vector")
  assert_that(is.numeric(channel_feature$POINT_Y),
              msg = "'POINT_Y' must be numeric vector")
  assert_that(is.numeric(channel_feature$POINT_M),
              msg = "'POINT_M' must be numeric vector")
}

bankline_points_test <- function(bankline_points) {
  assert_that(is.data.frame(bankline_points),
              msg = "'bankline_points' must be a data frame")
  assert_that("ReachName" %in% colnames(bankline_points),
              msg = "Required field 'ReachName' missing from 'bankline_points'")
  assert_that("POINT_X" %in% colnames(bankline_points),
              msg = "Required field 'POINT_X' missing from 'bankline_points'")
  assert_that("POINT_Y" %in% colnames(bankline_points),
              msg = "Required field 'POINT_Y' missing from 'bankline_points'")
  assert_that("POINT_M" %in% colnames(bankline_points),
              msg = "Required field 'POINT_M' missing from 'bankline_points'")
  assert_that("bank" %in% colnames(bankline_points),
              msg = "Required field 'bank' missing from 'bankline_points'")
  assert_that("DEM_Z" %in% colnames(bankline_points),
              msg = "Required field 'DEM_Z' missing from 'bankline_points'")
  assert_that("loop" %in% colnames(bankline_points),
              msg = "Required field 'loop' missing from 'bankline_points'")
  assert_that("bend" %in% colnames(bankline_points),
              msg = "Required field 'bend' missing from 'bankline_points'")
  assert_that("position" %in% colnames(bankline_points),
              msg = "Required field 'position' missing from 'bankline_points'")
  assert_that("v_POINT_X" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_X' missing from 'bankline_points'")
  assert_that("v_POINT_Y" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_Y' missing from 'bankline_points'")
  assert_that("v_POINT_M" %in% colnames(bankline_points),
              msg = "Required field 'v_POINT_M' missing from 'bankline_points'")
  assert_that(is.numeric(bankline_points$POINT_X),
              msg = "'POINT_X' must be numeric vector")
  assert_that(is.numeric(bankline_points$POINT_Y),
              msg = "'POINT_Y' must be numeric vector")
  assert_that(is.numeric(bankline_points$POINT_M),
              msg = "'POINT_M' must be numeric vector")
}

