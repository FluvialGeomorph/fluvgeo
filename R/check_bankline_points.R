#' @title Check the validity of an `fluvgeo` `bankline_points` data structure
#'
#' @description Checks that the input data structure `bankline_points` meets
#' the requirements for this data structure.
#'
#' @export
#' @param bankline_points   SpatialPointsDataFrame; a `bankline_points` data
#'                          structure used by the fluvgeo package.
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
  assert_that(nchar(unique(bankline_points@data$ReachName)[1]) > 0,
              msg = paste("Field `ReachName` is empty in", name))

  # Check that the `bank` field is populated
  assert_that(all(unique(bankline_points@data$bank) %in%
                   c("right descending", "left descending")),
              msg = paste("Field `bank` in", name, "must contain a `right
                          descending` bank and a `left descending` bank."))

  # Check each bankline is digitized from the downstream end to the upstream end
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

  # Print a diagnostic report of loops and bends
  print("Diagnostic report of bankline points")

  # Create a variable to hold the bank of the last loop
  last_loop_bank <- ""

  # Iterate through loops
  for(l in sort(unique(na.omit(bankline_points@data$loop)))) {
    print(paste("Loop", l))
    ## Subset for the current loop
    bl_pts_loop <- bankline_points@data[bankline_points@data$loop == l, ]

    ## Subset points without loop and bend assignments
    bl_pts_lb <- na.omit(bl_pts_loop)

    ## Check for apex point
    apex <- length(bl_pts_lb[bl_pts_lb$position == "apex", ]$position) > 0

    assert_that(apex == TRUE,
                msg = paste("Loop", l, "is missing an apex point.",
                            "Reminder: All points for a given loop must be",
                            "located along the same bankline."))

    ## Calculate apex point mean bank_POINT_M position
    apex_m <- mean(bl_pts_lb[bl_pts_lb$position == "apex", ]$bank_POINT_M)

    ## Get a vector of banks for the loop
    loop_bank <- na.omit(unique(bl_pts_loop$bank))
    print(paste("    Apex:", apex, "  Route-M:", round(apex_m, 2)))
    print(paste("    last loop bank:", last_loop_bank))
    print(paste("    current loop bank:", loop_bank))

    min_loop_m <- 0
    max_loop_m <- 0
    min_last_bend_m <- 0
    max_last_bend_m <- 0

    ## Iterate through bends
    for (b in sort(unique(bl_pts_lb[bl_pts_lb$position != "apex", ]$bend))) {
      print(paste("        Bend", b))
      ## Subset for the current bend
      bend_pts <- bl_pts_lb[bl_pts_lb$bend == b, ]

      ## Check for start and end points
      start <- length(bend_pts[bend_pts$position == "start", ]$position) > 0
      end   <- length(bend_pts[bend_pts$position == "end",   ]$position) > 0

      ## Throw errors if start or end points are missing
      assert_that(start == TRUE,
                  msg = paste("Loop", l, "Bend", b, "is missing a start point.",
                              "Reminder: All points for a given loop must be",
                              "located along the same bankline."))

      assert_that(end == TRUE,
                  msg = paste("Loop", l, "Bend", b, "is missing an end point.",
                              "Reminder: All points for a given loop must be",
                              "located along the same bankline."))

      ## Calculate start and end point mean bank_POINT_M position
      start_m <- mean(bend_pts[bend_pts$position == "start", ]$bank_POINT_M)
      end_m   <- mean(bend_pts[bend_pts$position == "end",   ]$bank_POINT_M)

      print(paste("            Start:", start, "Route-M:", round(start_m, 2)))
      print(paste("            End:",     end, "  Route-M:", round(end_m, 2)))

      ## Check that the end point is upstream of the start point
      assert_that(start_m < end_m,
                  msg = (paste("The Loop", l, "Bend", b, "start point is",
                               "upstream of the end point. Reminder: loop",
                               "points are delineated beginning at the",
                               "downstream end of the reach.")))

      ## Get a vector of banks for the bend
      bend_bank <- unique(bend_pts$bank)
      print(paste("            bank:", bend_bank))

      ## Check that all bend points are located on the same bank
      assert_that(length(bend_bank) == 1,
                msg = paste("Loop", l, "Bend", b,
                            "points must all be located on the same bank.",
                            "Reminder: All points for a given loop must be",
                            "located along the same bankline."))

      ## Check that all bend points are located on the same bank as the loop
      assert_that(all(loop_bank == bend_bank),
                  msg = paste("Loop", l, "Bend", b,
                              "points are not located on the same bank as",
                              "other points in the loop.",
                              "Reminder: All points for a given loop must be",
                              "located along the same bankline."))

      ## Update min and max loop and bend m-values
      if(b == 1) {
        min_loop_m      <- start_m
        max_loop_m      <- end_m
        min_last_bend_m <- min_loop_m
        max_last_bend_m <- max_loop_m
      }

      if(b > 1) {
        min_last_bend_m <- min_loop_m
        max_last_bend_m <- max_loop_m
        min_loop_m      <- min(c(min_loop_m, start_m))
        max_loop_m      <- max(c(max_loop_m, end_m))
      }

      print(paste("            min_loop_m:", round(min_loop_m, 2)))
      print(paste("            max_loop_m:", round(max_loop_m, 2)))
      print(paste("            min_last_bend_m:", round(min_last_bend_m, 2)))
      print(paste("            max_last_bend_m:", round(max_last_bend_m, 2)))

      ## Check if current bend start is upstream of last bend end
      if(b > 1) {
        assert_that(start_m >= max_last_bend_m,
                    msg = (paste("Loop", l, "Bend", b,
                                 "is not upstream of bend", b-1,
                                 "Reminder: Bends should be delineated in an",
                                 "upstream direction and must not overlap.")))

        print(paste("            ** Bend", b, "is upstream of Bend", b-1))
      }

    }
    ## Check if apex is within the loop
    assert_that(min_loop_m <= apex_m & apex_m <= max_loop_m,
                msg = (paste("The Loop", l, "apex point is not located within",
                             "the loop. Reminder: Verify that the apex point",
                             "is located between the downstream-most bend",
                             "and the upstream-most bend end points.")))

    print(paste("    ** The Loop", l, "apex point is located within the loop."))

    ## Check if all points in loop are on the same bank
    assert_that(length(loop_bank) == 1,
                msg = paste("Loop", l,
                            "points must all be located on the same bank.",
                            "Reminder: All points for a given loop must be",
                            "located along the same bankline."))

    ## Check bank of last loop does not match bank of current loop
    if(l > 1) {
      assert_that(loop_bank != last_loop_bank,
                  msg = paste("Loop", l, "points are located on the same bank",
                              "as the previous loop. Reminder: Loop points",
                              "must alternate banks from one loop to the next."))

      print(paste("    ** Loop", l, "points are located on the opposite bank from",
                  "Loop", l-1))
    }

    ## Update the last_loop_bank to the current loop_bank value
    last_loop_bank <- loop_bank
  }

  # Return TRUE if all assertions are met
  TRUE
}

