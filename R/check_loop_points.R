#' @title Check the validity of an `fluvgeo` `loop_points` data structure
#'
#' @description Checks that the input data structure `loop_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param loop_points   SpatialPointsDataFrame; a `loop_points` data structure
#'                      used by the fluvgeo package.
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
  assert_that(class(loop_points)[1] == "sf",
              msg = paste(name, "must be a sf object"))
  assert_that(is.data.frame(loop_points),
              msg = paste(name, "must be a data frame"))
  assert_that("ReachName" %in% colnames(loop_points) &
                is.character(loop_points$ReachName),
              msg = paste("Character field 'ReachName' missing from", name))
  assert_that("loop" %in% colnames(loop_points) &
                is.numeric(loop_points$loop),
              msg = paste("Numeric field 'loop' missing from", name))
  assert_that("bend" %in% colnames(loop_points) &
                is.numeric(loop_points$bend),
              msg = paste("Numeric field 'bend' missing from", name))
  assert_that("position" %in% colnames(loop_points) &
                is.character(loop_points$position),
              msg = paste("Character field 'position' missing from", name))

  # ReachName is not empty
  assert_that(nchar(unique(loop_points$ReachName)[1]) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check for duplicate or missing loops
  assert_that(length(unique(loop_points$loop)) ==
                length(min(loop_points$loop):max(loop_points$loop)),
              msg = paste("Check for duplicate or missing `loop` values in",
                          name))

  assert_that(all(unique(loop_points$loop) %in%
                                 min(loop_points$loop):max(loop_points$loop)
                 ),
              msg = paste("Check for duplicate or missing `loop` values in",
                          name))

  # Apex bend value must be set to zero
  assert_that(all(loop_points[loop_points$position == "apex", ]$bend) == 0,
              msg = paste("Field `bend` must be set to zero for apex points in",
                          name))

  # Position can only be one of "start", "end", of "apex"
  assert_that(all(unique(loop_points$position) %in%
                                               c("start", "end",  "apex" )),
              msg = paste("Field `position` must contain only values:",
                          "`start`, `end`, and `apex` in", name))

  # Print a diagnostic report of loops and bends
  print("Diagnostic report of loop points (count of records)")

  ## Iterate through loops
  for(l in sort(unique(loop_points$loop))) {
    print(paste("Loop", l))
    ## Subset for the current loop
    loop_pts <- loop_points[loop_points$loop == l, ]

    ## Check apex points
    apex_length <- length(loop_pts[loop_pts$position == "apex", ]$bend)
    print(paste("    Apex:", apex_length))
    assert_that(apex_length == 1,
                msg = paste("Loop", l, "must have one and only one apex point"))

    ## Remove apex points (bend == 0) before moving to bend iterator
    loop_pts_start_end <- loop_pts[loop_pts$position != "apex", ]

    ## Iterate through bends
    for (b in sort(unique(loop_pts_start_end$bend))) {
      print(paste("        Bend", b))
      # Subset for the current bend
      bend_pts <- loop_pts_start_end[loop_pts_start_end$bend == b, ]

      ## Check for at least one bend per loop
      ## #Compute summary statistics for minimum number of bends per loop
      lb_min <- aggregate(bend ~ loop, data = bend_pts, min)
      assert_that(all(lb_min$bend) == 1,
                  msg = paste("Loop", l, "must have at least one bend"))

      ## Check for bend start point
      start_length <- length(bend_pts[bend_pts$position == "start", ]$position)
      print(paste("            Start:", start_length))
      assert_that(start_length == 1,
                  msg = paste("Loop", l, "Bend", b,
                              "must have one and only one start point"))

      ## Check for bend end point
      end_length <- length(bend_pts[bend_pts$position == "end", ]$position)
      print(paste("            End:", end_length))
      assert_that(end_length == 1,
                  msg = paste("Loop", l, "Bend", b,
                              "must have one and only one end point"))
    }
  }

  # Return TRUE if all assertions are met
  TRUE
}
