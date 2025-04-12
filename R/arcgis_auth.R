#' Authenticate to an ArcGIS Enterprise Portal
#'
#' @description Authenticates to an ArcGIS Enterprise Portal.
#'
#' @return an httr token
#' @export
#'
#' @importFrom arcgisutils auth_client set_arc_token arc_token
#'
arcgis_auth <- function() {
  # create a token using a named user account
  client_token <- auth_client(
    client = Sys.getenv("ARCGIS_CLIENTID"),
    secret = Sys.getenv("ARCGIS_CLIENTSECRET"),
    host = Sys.getenv("ARCGIS_HOST"),
    expiration = 60)

  # set the arc_token for the current session
  set_arc_token(token = client_token)

  return(arc_token())
}
