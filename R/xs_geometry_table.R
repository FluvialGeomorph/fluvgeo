#' @title Calculate cross section hydraulic geometry metrics
#'
#' @description Calculates the hydraulic geometry metrics bankfull (width,
#'     depth, area, elevation), floodprone (width, depth, area, elevation),
#'     width-depth ratio, entrenchment ratio, and water surface elevation for
#'     the input cross section.
#'
#' @export
#' @param xs_points           character; a data frame of cross section points
#' @param stream              character; The name of the stream.
#' @param xs_number           integer; The cross section identifier of the
#'                            requested cross section.
#' @param bankfull_elevation  numeric; The detrended bankfull elevation (in
#'                            feet) that is used to calculate hydraulic
#'                            geometry.
#'
#' @return A data frame of hydraulic dimensions for the specified cross section
#'    at the specified detrended bankfull elevation.
#'    \describe{
#'        \item{reach_name}{character; The name of the stream.}
#'        \item{cross_section}{numeric; The cross section unique identifier.
#'                         Seq is only unique within a reach.}
#'        \item{xs_type}{character; A string indicating how the cross section
#'                       was derived. "DEM derived cross section" denotes
#'                       dimensions calculated from the DEM and "<Region
#'                       Name>" denotes that the dimensions were calculated
#'                       from that regions regional curve.}
#'        \item{bankfull_elevation}{numeric; The detrended bankfull elevation
#'                       (in feet) that is used to calculate hydraulic
#'                       geometry.}
#'        \item{drainage_area}{numeric; The area of the watershed upstream
#'                       from this cross section in square miles.}
#'        \item{xs_area}{numeric; The cross sectional area at the specified
#'                       detrended bankfull elevation.}
#'        \item{xs_width}{numeric; The cross section width at the specified
#'                        detrended bankfull elevation.}
#'        \item{xs_depth}{numeric; The maximum depth at the specified
#'                        detrended bankfull elevation.}
#'        \item{fp_area}{numeric; The cross sectional area at the flood prone
#'                       elevation.}
#'        \item{fp_width}{numeric; The cross section width at the flood prone
#'                        elevation.}
#'        \item{fp_depth}{numeric; The maximum depth at the flood prone
#'                        elevation.}
#'        \item{xs_width_depth_ratio}{numeric; The bankfull width to bankfull
#'                        depth ratio.}
#'        \item{xs_entrenchment_ratio}{numeric; The entrenchment ratio (flood
#'                        prone width) / (bankfull width).}
#'        \item{watersurface_elev}{numeric; The water surface elevation.}
#'        \item{bankfull_elev}{numeric; The bankfull elevation.}
#'        \item{floodprone_elev}{numeric; The flood prone elevation.}
#'    }
#'
#' @details Add methods description for calculating flood prone, width-depth
#'     ratio, and entrenchment ratio.
#'
#' @importFrom stats na.omit
#'
xs_geometry_table <- function(xs_points, stream, xs_number,
                             bankfull_elevation) {
  # Subset xs_points for the current cross section
  print(xs_number)
  xs <- na.omit(xs_points[xs_points$ReachName == stream &
                            xs_points$Seq == xs_number,])
  # Determine drainage area
  drainageArea <- unique(xs$Watershed_Area_SqMile)
  # Calculate cross section geometry at bankfull
  xs_dims <- xs_geometry(xs, bankfull_elevation)
  print(paste0("    ", bankfull_elevation))
  # Calculate cross section geometry at flood-prone
  fp_elevation <- bankfull_elevation + (bankfull_elevation - 100)
  print(paste0("    ", fp_elevation))
  fp_dims <- xs_geometry(xs, fp_elevation)
  # Calculate width-depth ratio
  xs_width_depth <- xs_dims$xs_width / xs_dims$xs_depth
  # Calculate entrenchment ratio
  xs_entrench <- fp_dims$xs_width / xs_dims$xs_width
  # Calculate Water surface elevation
  watersurface_elev <- xs[xs$Detrend_DEM_Z == min(xs$Detrend_DEM_Z),]$DEM_Z
  # Define type of cross section
  xs_type <- c("DEM derived cross section")
  # Build data frame of results
  dims <- data.frame(stream, xs_number, xs_type, bankfull_elevation,
                     drainageArea, xs_dims$xs_area, xs_dims$xs_width,
                     xs_dims$xs_depth, fp_dims$xs_area, fp_dims$xs_width,
                     fp_dims$xs_depth, xs_width_depth, xs_entrench,
                     watersurface_elev, xs_dims$ground_elev,
                     fp_dims$ground_elev)
  colnames(dims) <- c("reach_name","cross_section","xs_type",
                      "bankfull_elevation","drainage_area","xs_area",
                      "xs_width","xs_depth","fp_area","fp_width","fp_depth",
                      "xs_width_depth_ratio","xs_entrenchment_ratio",
                      "watersurface_elev","bankfull_elev",
                      "floodprone_elev")
  return(dims)
}
