#' @title Check that the `fgm` data structure is oriented down hill
#'
#' @description Checks that the input data structure is digitized downhill.
#'
#' @export
#' @param fgm_feature       data frame; an `fgm` data structure.
#'
#' @details This function checks that `fgm` features (e.g. flowline,
#' cross sections, banklines, centerline, etc.) are digitized from the bottom
#' of the reach to the top of the reach (not in the direction of flow).
#'
#' @return Returns TRUE if the `fgm_feature` data structure matches the
#' requirements. The function throws an error for a data structure not matching
#' the data specification. Returns errors describing how the the data structure
#' doesn't match the requirement.
#'
#' @importFrom assertthat assert_that
#'
check_downhill <- function(fgm_feature) {
  name <- deparse(substitute(fgm_feature))

  # Test for field names and Calculate min_z and max_z values
  if("Z" %in% colnames(fgm_feature) &
     "POINT_M" %in% colnames(fgm_feature)) {
    # Get min and max POINT_M value
    m_min <- min(fgm_feature$POINT_M)
    m_max <- max(fgm_feature$POINT_M)

    # Calculate min and max z
    m_min_z <- min(fgm_feature[fgm_feature$POINT_M == m_min, ]$Z)
    m_max_z <- max(fgm_feature[fgm_feature$POINT_M == m_max, ]$Z)
  }

  if ("DEM_Z" %in% colnames(fgm_feature) &
      "bank_POINT_M" %in% colnames(fgm_feature)) {
    # Get min and max POINT_M value
    m_min <- min(fgm_feature$bank_POINT_M)
    m_max <- max(fgm_feature$bank_POINT_M)

    # Calculate min and max z
    m_min_z <- min(fgm_feature[fgm_feature$bank_POINT_M == m_min, ]$DEM_Z)
    m_max_z <- max(fgm_feature[fgm_feature$bank_POINT_M == m_max, ]$DEM_Z)
  }


  assert_that(is.data.frame(fgm_feature),
              msg = paste(name, "must be a data frame"))
  assert_that((("POINT_M" %in% colnames(fgm_feature)) |
               ("bank_POINT_M" %in% colnames(fgm_feature))) &
               is.numeric(fgm_feature$POINT_M) |
               is.numeric(fgm_feature$bank_POINT_M) ,
              msg = paste("Numeric field 'POINT_M' or 'bank_POINT_M' missing from", name))
  assert_that((("Z" %in% colnames(fgm_feature)) |
               ("DEM_Z" %in% colnames(fgm_feature))) &
               is.numeric(fgm_feature$Z) |
                is.numeric(fgm_feature$DEM_Z),
              msg = paste("Numeric field 'Z' or 'DEM_Z' missing from", name))
  assert_that(m_min_z < m_max_z,
              msg = paste("feature is not digitized in the upstream direction", name))
}
