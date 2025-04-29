#' @title Get Watershed
#' @description Get watershed from ESRI geoprocessing service.
#' @param pour_point   sf object; An sf pour point object.
#' @returns An sf polygon object representing the upstream drainage area of
#'          the pour point.
#' @export
#'
#' @importFrom httr2 request req_url_query req_perform
#' @importFrom arcgisutils as_esri_featureset
#' @importFrom sf st_point st_sfc st_as_sf
#' @importFrom dplyr mutate
#'
get_watershed <- function(pour_point) {

# authenticate to AGOL
token <- arcgis_auth()

# Submit watershed request
watershed_submitJob_url <- "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/submitJob"
# https://developers.arcgis.com/rest/elevation-analysis/watershed/
watershed_submit <- request(watershed_submitJob_url) |>
  req_headers(Authorization = token) |>
  req_url_query("InputPoints" = as_esri_featureset(pour_point),
                "PointIDField" = "PointID",
                "SnapDistance" = 5,
                "SnapDistanceUnits" = "Meters",
                "DataSourceResolution" = "FINEST",
                "Generalize" = "True",
                "ReturnSnappedPoints" = "True") |>
  req_perform()
watershed_submit

# Retrieve results
jobId <- watershed_submit$headers$ETag
yourToken <- ""

watershed_jobs_url <- "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/jobs"
job_status <- request(watershed_jobs_url) |>
  req_url_query("jobId" = jobId) |>
  req_perform()
job_status




watersheds_fs <- st_read



}
