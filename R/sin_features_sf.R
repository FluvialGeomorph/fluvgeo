#' @docType data
#'
#' @title Sinsinawa Creek, IL stream features
#'
#' @description  A \code{sf} object containing stream
#' feature points used to identify and label key stream features and their
#' longitudinal position. Instances of features typically include relevant
#' tributaries, localities within the floodplain affecting flow.
#'
#' @format A \code{sf} with 5 observations and 4
#'     variables.
#' \describe{
#'     \item{OBJECTID}{numeric; The ArcGIS feature class assigned unique
#'                     identifier for each point in the dataset.}
#'     \item{Name}{character; The name of the stream feature. }
#'     \item{km_to_mouth}{numeric; The longitudinal position of the feature.}
#' }
#' @source This dataset was produced using the \code{FluvialGeomorph} ArcGIS
#' toolbox.
#'
#' @keywords datasets
#'
"sin_features_sf"
