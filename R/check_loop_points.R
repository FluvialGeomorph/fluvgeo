#' @title Check the validity of an `fgm` `loop_points` data structure
#'
#' @description Checks that the input data structure `loop_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param loop_points   data frame; a `loop_points` data structure used
#'                        by the fgm package.
#'
#' @details Cross section feature classes evolve as different steps are
#' performed on them. The `step` parameter allows a `loop_points` data
#' structure to be checked throughout its lifecycle. Each step defines a
#' changing set of requirements for the `loop_points` data structure.
#'
#' @return Returns TRUE if the `loop_points` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_loop_points <- function(loop_points) {
  name <- deparse(substitute(loop_points))

  # Check data structure
  assert_that(class(loop_points)[1] == "SpatialPointsDataFrame",
              msg = paste(name, "must be a SpatialPointsDataFrame"))
  assert_that(is.data.frame(loop_points@data),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(loop_points@data) &
                is.character(loop_points@data$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("loop" %in% colnames(loop_points@data) &
                is.numeric(loop_points@data$loop),
              msg = paste("Numeric field 'loop' missing from", name))
  assert_that("bend" %in% colnames(loop_points@data) &
                is.numeric(loop_points@data$bend),
              msg = paste("Numeric field 'bend' missing from", name))
  assert_that("position" %in% colnames(loop_points@data) &
                is.character(loop_points@data$position),
              msg = paste("Character field 'position' missing from", name))

  # ReachName is not empty
  assert_that(nchar(unique(loop_points$ReachName[1])) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Bends must have one and only one start and end loop point
  for(l in unique(loop_points$loop)) {
    # subset for current loop and exclude apex points
    cl <- loop_points[loop_points$loop == l & loop_points$position != "apex", ]
    for(b in unique(cl$bends)) {
      # subset for current bend
      cl_b <- cl[cl$bend == b, ]
      # check that there is one and only one start loop point
      assert_that(length(cl_b[cl_b$position == "start", ]$position) == 1,
                  msg = paste("loop:", l, "bend:", b,
                              "Must have one and only one start loop point"))
      # check that there is one and only one end loop point
      assert_that(length(cl_b[cl_b$position == "end", ]$position) == 1,
                  msg = paste("loop:", l, "bend:", b,
                              "Must have one and only one end loop point"))
    }
  }

  # Check for duplicate or missing loops
  assert_that(length(unique(loop_points$loop)) ==
                length(min(loop_points$loop):max(loop_points$loop)),
              msg = paste("Check for duplicate or missing `loop` values in", name))

  # Apex bend value must be set to zero
  assert_that(all(loop_points[loop_points$position == "apex", ]$bend) == 0,
              msg = paste("Field `bend` must be set to zero for apex points in",
                          name))

  # Every loop must have one and only apex point
  assert_that(length(unique(loop_points$loop)) ==
                length(loop_points[loop_points$position == "apex", ]$loop),
              msg = paste("Every loop must have one and only one apex point in",
                          name))

  # Position can only be one of "start", "end", of "apex"
  assert_that(all(unique(loop_points$position) == c("start", "end",  "apex" )),
              msg = paste("Field `position` must contain only values:",
                          "`start`, `end`, and `apex` in", name))
}
