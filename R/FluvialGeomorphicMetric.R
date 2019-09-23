#' An S4 class to represent a fluvial geomorphic metric
#'
#' @slot metric            name of the metric
#' @slot definition        mathmatical definition of the metric
#' @slot threshold_breaks  numeric breaks of thresholds
#' @slot threshold_labels  text description of the catagories defined by
#'                         threshold_breaks
#' @slot source            citation for the thresholds
#'
setClass(Class = "FluvialGeomorphicMetric",
         slots = list(metric = "character",
                      definition = "character",
                      threshold_breaks = "vector",
                      threshold_labels = "vector",
                      variable = "character",
                      source = "character"))

