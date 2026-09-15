#' Locate a nearby mapped stream for drainage exploration
#'
#' Uses hydrogeofetch's NLDI hydrolocation service. Only results within 200 metres
#' of the supplied point are accepted: more distant raindrop-trace results are
#' refused rather than described as nearest-stream snaps. This is NHDPlusV2
#' reference hydrography, not a surveyed channel or a terrain-derived FG network.
#'
#' @param point One nonempty sf or sfc POINT with a known CRS.
#' @return A `fg_drainage_location` list with `query_point`, `snapped_point`,
#'   `flowline` (sf in EPSG:4326), `comid`, `snap_distance_m`, `retrieved_at`, and
#'   `method`. Service failure or an unusable/ambiguous location raises an error.
#' @export
locate_drainage_stream <- function(point) {
  point <- drainage_point(point)
  index <- drainage_response(function() drainage_index_service(
    unname(sf::st_coordinates(point)[1, 1:2])), "POINT")
  if (!"comid" %in% names(index)) stop("No mapped stream was identified.", call. = FALSE)
  index <- index[!is.na(index$comid) & grepl("^[0-9]+$", index$comid), ]
  if (nrow(index) != 1L) stop("No unique stream snap returned. Choose a point closer to the intended channel.", call. = FALSE)
  distance <- as.numeric(sf::st_distance(point, index))
  if (length(distance) != 1L || !is.finite(distance) || distance > 200)
    stop("No stream snap within 200 metres. Zoom in and click closer to a mapped channel; downstream tracing is not used here.", call. = FALSE)
  comid <- as.character(index$comid[[1]])
  line <- drainage_response(function() drainage_feature_service(comid), c("LINESTRING", "MULTILINESTRING"))
  if (!"comid" %in% names(line) || any(as.character(line$comid) != comid))
    stop("The returned channel does not match the snapped stream.", call. = FALSE)
  structure(list(query_point = point, snapped_point = index, flowline = line,
    comid = comid, snap_distance_m = distance,
    retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    method = "NLDI hydrolocation; nearest mapped stream within 200 m"),
    class = "fg_drainage_location")
}

#' Retrieve candidate drainage layers for a reviewed stream location
#'
#' Retrieves WBD 2025 HUC12 polygons containing the snapped point, an NLDI
#' catchment-based upstream basin, and distance-limited upstream tributaries and
#' downstream mainstem. Basin geometry is simplified for exploration, is NOT
#' split at the clicked point, and is not an exact pour-point delineation.
#' Network completeness is not established by these distance-limited requests.
#' These are reference candidates, not FG Study Area, Stream or Reach records.
#' No project files are read or written. Coordinates are sent to USGS services.
#'
#' @param location A result of [locate_drainage_stream()].
#' @param distance_km Navigation distance in kilometres, from 1 to 200, for each
#'   network direction. The basin and HUC12 queries do not use this distance.
#' @param include_names Retrieve optional GNIS channel names with one attribute-only
#'   Fabric query for up to 500 distinct returned COMIDs. Defaults to FALSE.
#' @return A list with `location`, `layers` (named sf or NULL), `status` (one row
#'   per layer: layer, status, features, detail, outcome), `distance_km`, `retrieved_at`,
#'   `sources`, and `network_complete` (NA). Individual layer failures are retained
#'   as unavailable without discarding other results or asserting no coverage.
#'   `outcome` distinguishes `available`, `no_features` (an explicit empty sf
#'   response or no containing HUC), `service_unavailable` (transport evidence),
#'   and `unresolved` (including NULL with no retained transport evidence).
#'   `name_lookup` records optional name-query status and details; name-query
#'   failure never discards successfully retrieved geometry.
#' @export
get_drainage_context <- function(location, distance_km = 50, include_names = FALSE) {
  if (!is.logical(include_names) || length(include_names) != 1L || is.na(include_names))
    stop("include_names must be TRUE or FALSE.", call. = FALSE)
  if (!inherits(location, "fg_drainage_location") || length(location$comid) != 1L ||
      is.na(location$comid) || !grepl("^[0-9]+$", location$comid))
    stop("Supply a located stream from locate_drainage_stream().", call. = FALSE)
  point <- drainage_point(location$snapped_point)
  if (!is.numeric(distance_km) || length(distance_km) != 1L ||
      !is.finite(distance_km) || distance_km < 1 || distance_km > 200)
    stop("Navigation distance must be between 1 and 200 kilometres.", call. = FALSE)
  calls <- list(
    huc12 = function() {
      x <- drainage_geometry(drainage_huc_service(point), c("POLYGON", "MULTIPOLYGON"))
      x[lengths(sf::st_intersects(x, point)) > 0L, ]
    },
    basin = function() drainage_basin_service(location$comid),
    upstream = function() drainage_navigation_service(location$comid, "UT", distance_km),
    downstream = function() drainage_navigation_service(location$comid, "DM", distance_km))
  layers <- stats::setNames(vector("list", length(calls)), names(calls))
  rows <- vector("list", length(calls))
  for (i in seq_along(calls)) {
    key <- names(calls)[i]
    allowed <- if (key %in% c("huc12", "basin")) c("POLYGON", "MULTIPOLYGON") else c("LINESTRING", "MULTILINESTRING")
    detail <- "Returned reference features; not adopted into a study."
    outcome <- "available"
    value <- tryCatch(drainage_response(calls[[i]], allowed), error = function(e) {
      detail <<- if (is.null(e$details)) conditionMessage(e) else e$details
      outcome <<- if (is.null(e$code)) "unresolved" else e$code
      NULL
    })
    layers[i] <- list(value)
    rows[[i]] <- data.frame(layer = key, status = if (is.null(value)) "unavailable" else "available",
      features = if (is.null(value)) 0L else nrow(value), detail = detail, outcome = outcome)
  }
  named <- if (include_names) drainage_channel_names(layers) else
    list(layers = layers, status = "not_requested", detail = "Optional channel names not requested.")
  list(location = location, layers = named$layers, status = do.call(rbind, rows),
    name_lookup = named[c("status", "detail")],
    distance_km = distance_km, network_complete = NA,
    retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    sources = c(network = "NLDI / NHDPlusV2", huc12 = "USGS WBD 2025",
      client = paste0("hydrogeofetch ", utils::packageVersion("hydrogeofetch"))))
}

# Optional enrichment never substitutes geometry or joins by name.
drainage_channel_names <- function(layers) {
  keys <- intersect(c("upstream", "downstream"), names(layers))
  id_field <- function(x) intersect(c("nhdplus_comid", "comid"), names(x))[1]
  ids <- unique(unlist(lapply(layers[keys], function(x) {
    field <- id_field(x)
    if (!is.na(field)) as.character(x[[field]]) else character()
  }), use.names = FALSE))
  ids <- ids[!is.na(ids) & grepl("^[0-9]+$", ids)]
  status <- "not_requested"
  detail <- "No channel identifiers available for name lookup."
  if (!length(ids)) return(list(layers = layers, status = status, detail = detail))
  if (length(ids) > 500L) return(list(layers = layers, status = "skipped",
    detail = "Name lookup skipped above 500 distinct channels to bound preview requests; reduce search distance to retrieve names."))
  warnings <- character()
  names <- tryCatch(withCallingHandlers(drainage_names_service(ids), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) { warnings <<- c(warnings, conditionMessage(e)); NULL })
  if (!is.data.frame(names) || !all(c("comid", "gnis_name") %in% colnames(names)) ||
      anyDuplicated(as.character(names$comid))) {
    return(list(layers = layers, status = "unresolved", detail = paste(c(
      "Channel names unavailable; source identifiers and geometry retained.", warnings), collapse = "\n")))
  }
  for (key in keys) {
    x <- layers[[key]]
    field <- id_field(x)
    if (is.na(field)) next
    values <- trimws(as.character(names$gnis_name[match(as.character(x[[field]]), as.character(names$comid))]))
    values[is.na(values) | !nzchar(values)] <- NA_character_
    x$gnis_name <- values
    layers[[key]] <- x
  }
  list(layers = layers, status = "returned", detail = paste0(
    "USGS Fabric NHDPlusV2 GNIS names joined by COMID (attribute-only); ",
    sum(ids %in% as.character(names$comid)), " of ", length(ids),
    " identifiers returned. Blank names remain unspecified.",
    if (length(warnings)) paste0(" ", paste(warnings, collapse = "; ")) else ""))
}

drainage_point <- function(point) {
  if (inherits(point, "sfc")) point <- sf::st_sf(geometry = point)
  point <- drainage_geometry(point, "POINT")
  if (nrow(point) != 1L) stop("Supply exactly one point.", call. = FALSE)
  point
}

drainage_geometry <- function(x, types) {
  if (inherits(x, "sf") && nrow(x) == 0L)
    stop(errorCondition("No matching features in the returned result.",
      class = "fg_drainage_error", code = "no_features"))
  if (!inherits(x, "sf") || nrow(x) == 0L)
    stop("No usable features returned; service failure or absent coverage is possible.", call. = FALSE)
  if (nrow(x) > 10000L) stop("Too many features for this exploration preview. Reduce the navigation distance.", call. = FALSE)
  if (is.na(sf::st_crs(x)) || any(sf::st_is_empty(x)) ||
      !all(as.character(sf::st_geometry_type(x)) %in% types))
    stop("Unexpected geometry or missing CRS in the response.", call. = FALSE)
  x <- sf::st_transform(x, 4326)
  coords <- sf::st_coordinates(x)
  if (nrow(coords) > 1000000L || any(!is.finite(coords[, 1:2])) ||
      any(abs(coords[, 1]) > 180) || any(abs(coords[, 2]) > 90))
    stop("Response coordinates are invalid or too large for this preview.", call. = FALSE)
  if (!all(sf::st_is_valid(x) %in% TRUE))
    stop("Invalid geometry returned; no automatic repair was applied.", call. = FALSE)
  x
}

# Keep per-request evidence before permissive upstream clients can collapse an
# error and an empty result to the same NULL. Never interpret NULL as no coverage.
drainage_response <- function(fetch, types) {
  warnings <- character()
  tryCatch(withCallingHandlers(drainage_geometry(fetch(), types), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    detail <- paste(c(conditionMessage(e), warnings), collapse = "\n")
    transport <- grepl("HTTP [45][0-9][0-9]|[Tt]imed? ?out|[Tt]imeout|[Cc]ould not resolve|[Ff]ailed to connect|[Cc]onnection", detail)
    code <- if (transport) "service_unavailable" else if (identical(e$code, "no_features")) "no_features" else "unresolved"
    stop(errorCondition(conditionMessage(e), class = "fg_drainage_error", code = code, details = detail))
  })
}

# Small service seams for deterministic tests; do not follow response-supplied URLs.
drainage_index_service <- function(x) hydrogeofetch::get_nldi_index(x)
drainage_names_service <- function(ids) hydrogeofetch::get_nhdplus(comid = ids,
  properties = c("comid", "gnis_name"), skip_geometry = TRUE)
drainage_feature_service <- function(id) hydrogeofetch::get_nldi_feature(list(featureSource = "comid", featureID = id))
drainage_huc_service <- function(point) hydrogeofetch::get_huc(AOI = point, type = "huc12_2025", t_srs = 4326)
drainage_basin_service <- function(id) hydrogeofetch::get_nldi_basin(list(featureSource = "comid", featureID = id), simplify = TRUE, split = FALSE)
drainage_navigation_service <- function(id, mode, distance) {
  x <- hydrogeofetch::navigate_nldi(list(featureSource = "comid", featureID = id), mode = mode,
    data_source = "flowlines", distance_km = distance)
  x[[paste0(mode, "_flowlines")]]
}
