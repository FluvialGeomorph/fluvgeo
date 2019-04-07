#' @docType data
#'
#' @title Sinachwine Creek, IL cross section dimensions
#'
#' @description  A \code{sp::SpatialLinesDataFrame} object containing stream
#' cross sections with calculated geomorphic dimensions (Seq 1 - 10).
#'
#' @format A \code{SpatiallinesDataFrame} with 10 observations and 22
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Seq}{numeric; The cross section unique identifier. Seq is only
#'                unique within a reach.}
#'     \item{Watershed_Area_SqMile}{numeric; The area of the watershed upstream
#'                    from this cross section in square miles.}
#'     \item{from_measure}{numeric; The route measurement start value. }
#'     \item{to_measure}{numeric; The route mesurement end value. }
#'     \item{ReachName}{character; The name of the stream. }
#'     \item{km_to_mouth}{numeric; Distance downstream from this cross section
#'                    to the end of the reach.}
#'     \item{reach_name}{character; The name of the stream (snake case). }
#'     \item{xs_type}{character; The source of the cross section dimensions. One
#'                    of "DEM derived cross section" or "regional curve derived
#'                    cross section". }
#'     \item{bankfull_elevation}{numeric; The detrended bankfull elevation of
#'                               the cross section. }
#'     \item{drainage_area}{numeric; The area of the watershed upstream
#'                    from this cross section in square miles.}
#'     \item{xs_area}{numeric; The cross sectional area. }
#'     \item{xs_width}{numeric; The cross section width. }
#'     \item{xs_depth}{numeric; the crossectional depth. }
#'     \item{fp_area}{numeric; The floodprone area. The floodprone area is
#'                    calculated from a water surface elevtion two times the
#'                    bankfull elevation. }
#'     \item{fp_width}{numeric; The floodprone width. The floodprone width is
#'                    calculated from a water surface elevation two times the
#'                    bankfull elevation. }
#'     \item{fp_depth}{numeric; The floodprone depth. The floodprone depth is
#'                    calculated from a water surface elevation two times the
#'                    bankfull elevation. }
#'     \item{xs_width_depth_ratio}{numeric; The ratio of the bankfull width to
#'                                 the bankfull depth. }
#'     \item{xs_entrenchment_ratio}{numeric; The entrenchment ratio calculated
#'                                  as the ratio between the bankfull width
#'                                  divided by the floodprone width. }
#'     \item{watersurface_elev}{numeric; The water surface elevation. }
#'     \item{bankfull_elev}{numeric; The bankfull elevation. }
#'     \item{floodprone_elev}{numeric; The floodprone elevation. }
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_xs_dimensions"
