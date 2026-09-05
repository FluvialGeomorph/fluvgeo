#' Assign candidate nodes and derive hydrologic connectivity
#'
#' Uses exact shared endpoints to construct a downstream-oriented sfnetwork
#' copy, then hydroloom to derive and order all downstream connections. Storage
#' geometry remains downstream to upstream and is never changed. Diversions
#' retain every connection; no main path or segment role is inferred.
#'
#' All directions must already be CONFIRMED. Closed/self-intersecting lines,
#' duplicates, overlaps, interior intersections, and directed cycles block this
#' operation. Near endpoints remain separate; there is no snapping or tolerance.
#' Disconnected components and multiple outlets are permitted and remain subject
#' to observation review. A boundary is only the edge of the observed network,
#' not evidence of a physical headwater or mouth. This is not acceptance validation.
#'
#' @param stream_network Nonempty projected XY LINESTRING sf with unique UUID
#'   `stream_network_segment_id`, one `stream_network_observation_id`, and
#'   `direction_status`. Coordinate order must be downstream to upstream.
#'   Optional `upstream_node_id`/`downstream_node_id` UUIDs are reused when
#'   consistent with exact endpoint sharing; missing identities are generated.
#'   Different IDs at one endpoint, or one ID at different endpoints, are errors.
#'
#' @return A list with `stream_network` (input attributes/geometry with node FKs),
#'   `stream_network_node` (POINT sf with node/observation UUIDs, `in_degree`,
#'   `out_degree`, `node_topology`), and `stream_network_connection` (tibble).
#'   Connections contain observation UUID, upstream `stream_network_segment_id`,
#'   nullable `downstream_segment_id`, and `node_id` at their shared endpoint.
#'   Each observed outlet has one row with a missing downstream segment. Rows
#'   follow hydroloom's upstream-to-downstream ordering, with repeated segment
#'   IDs at diversions. Node IDs are observation-scoped candidate identities,
#'   not cross-observation matches. Preserve returned node FKs for repeat calls.
#' @export
connect_stream_network <- function(stream_network) {
  x <- stream_network
  required <- c("stream_network_segment_id", "stream_network_observation_id",
                "direction_status")
  if (!inherits(x, "sf") || !nrow(x) || !all(required %in% names(x)) ||
      is.na(sf::st_crs(x)) || isTRUE(sf::st_is_longlat(x))) {
    .fg_abort("`stream_network` must be nonempty projected sf with segment/observation IDs and direction_status.")
  }
  ids <- .fg_uuid(x$stream_network_segment_id, "stream_network_segment_id")
  observations <- .fg_uuid(x$stream_network_observation_id, "stream_network_observation_id")
  if (anyDuplicated(ids) || length(unique(observations)) != 1L) {
    .fg_abort("Connectivity requires unique segment IDs in exactly one observation.")
  }
  fail <- function(code, message) rlang::abort(
    message, class = "fluvgeo_connectivity_error", issue_code = code
  )
  if (anyNA(x$direction_status) || any(x$direction_status != "CONFIRMED")) {
    fail("CONNECTIVITY_DIRECTION_UNRESOLVED", "Connectivity requires CONFIRMED direction on every segment; no partial network was assigned.")
  }
  geometry <- sf::st_geometry(x)
  if (any(sf::st_geometry_type(x) != "LINESTRING") ||
      any(vapply(geometry, function(g) !inherits(g, "XY"), logical(1))) ||
      any(sf::st_is_empty(x)) || anyNA(sf::st_is_valid(x)) ||
      any(!sf::st_is_valid(x)) || any(as.numeric(sf::st_length(x)) <= 0)) {
    .fg_abort("Connectivity requires valid, positive-length XY LINESTRING geometry.")
  }
  closed <- vapply(geometry, function(g) all(g[1L, ] == g[nrow(g), ]), logical(1))
  if (any(!sf::st_is_simple(x)) || any(closed)) {
    fail("CONNECTIVITY_GEOMETRY_UNRESOLVED", "Closed or self-intersecting geometry requires review before node assignment.")
  }
  # Spatially indexed exact intersections; do not infer a junction at a crossing.
  intersections <- sf::st_intersects(x)
  for (i in seq_len(nrow(x))) {
    for (j in intersections[[i]][intersections[[i]] > i]) {
      relation <- sf::st_relate(geometry[i], geometry[j])[1L, 1L]
      if (any(substring(relation, c(1L, 2L, 4L), c(1L, 2L, 4L)) != "F")) {
        fail("CONNECTIVITY_GEOMETRY_UNRESOLVED", "Duplicate, overlapping, or interior-intersecting geometry requires review before node assignment.")
      }
    }
  }
  # Supply only computational fields so legacy from/to columns cannot leak into
  # the graph, and reverse only a copy of the downstream-to-upstream storage.
  net <- sfnetworks::as_sfnetwork(sf::st_sf(
    id = ids, geometry = sf::st_reverse(geometry)), directed = TRUE)
  if (!igraph::is_dag(net)) {
    fail("CONNECTIVITY_DIRECTED_CYCLE", "Directed cycle requires review; hydrologic ordering and node assignment were not applied.")
  }
  edges <- sf::st_as_sf(net, active = "edges")
  points <- sf::st_as_sf(net, active = "nodes")
  # New IDs only for previously unidentified endpoints. Existing IDs must agree
  # with both spatial sharing and uniqueness; graph positions never become IDs.
  existing <- lapply(c("upstream_node_id", "downstream_node_id"), function(field) {
    if (!field %in% names(x)) return(rep(NA_character_, nrow(x)))
    .fg_optional_uuid_vector(x[[field]], field, nrow(x))
  })
  endpoint_ids <- unlist(existing, use.names = FALSE)
  endpoint_nodes <- c(edges$from, edges$to)
  node_ids <- vapply(seq_len(nrow(points)), function(i) {
    known <- unique(endpoint_ids[endpoint_nodes == i & !is.na(endpoint_ids)])
    if (length(known) > 1L) {
      fail("NODE_IDENTITY_CONFLICT", "Conflicting node IDs at one exact shared endpoint.")
    }
    if (length(known)) known else NA_character_
  }, character(1))
  if (anyDuplicated(node_ids[!is.na(node_ids)])) {
    fail("NODE_IDENTITY_CONFLICT", "One node ID cannot identify different endpoint locations.")
  }
  node_ids[is.na(node_ids)] <- .fg_generate_uuid(sum(is.na(node_ids)))
  x$stream_network_segment_id <- ids
  x$stream_network_observation_id <- observations
  x$upstream_node_id <- node_ids[edges$from]
  x$downstream_node_id <- node_ids[edges$to]
  incoming <- as.integer(igraph::degree(net, mode = "in"))
  outgoing <- as.integer(igraph::degree(net, mode = "out"))
  kind <- rep("COMPLEX_JUNCTION", nrow(points))
  kind[incoming == 1L & outgoing == 1L] <- "CONTINUATION"
  kind[incoming > 1L & outgoing == 1L] <- "CONFLUENCE"
  kind[incoming == 1L & outgoing > 1L] <- "DIVERGENCE"
  kind[incoming == 0L] <- "UPSTREAM_BOUNDARY"
  kind[outgoing == 0L] <- "DOWNSTREAM_BOUNDARY"
  nodes <- sf::st_sf(tibble::tibble(
    node_id = node_ids, stream_network_observation_id = rep(observations[1L], length(node_ids)),
    in_degree = incoming, out_degree = outgoing, node_topology = kind
  ), Shape = sf::st_geometry(points))
  # add_toids(FALSE) is hydroloom's public non-dendritic node-to-edge adapter.
  # It retains all divergence rows without requiring an invented main path.
  hy <- hydroloom::hy(data.frame(
    id = ids, fromnode = x$upstream_node_id, tonode = x$downstream_node_id
  ))
  flow <- hydroloom::sort_network(hydroloom::add_toids(hy, return_dendritic = FALSE))
  downstream <- as.character(flow$toid)
  # hydroloom uses an empty text outlet sentinel for character feature IDs.
  downstream[!is.na(downstream) & downstream == ""] <- NA_character_
  if (any(!is.na(downstream) & !downstream %in% ids) || !setequal(flow$id, ids)) {
    .fg_abort("Hydroloom connectivity did not preserve the input segment identities.")
  }
  connections <- tibble::tibble(
    stream_network_observation_id = rep(observations[1L], nrow(flow)),
    stream_network_segment_id = as.character(flow$id),
    downstream_segment_id = downstream,
    node_id = x$downstream_node_id[match(flow$id, ids)]
  )
  list(stream_network = x, stream_network_node = nodes,
       stream_network_connection = connections)
}

# Typed optional preparation outputs, including deferred/validate-only calls.
.fg_empty_connectivity <- function(crs) {
  list(
    stream_network_node = sf::st_sf(tibble::tibble(
      node_id = character(), stream_network_observation_id = character(),
      in_degree = integer(), out_degree = integer(), node_topology = character()
    ), Shape = sf::st_sfc(sf::st_point(), crs = crs)[0]),
    stream_network_connection = tibble::tibble(
      stream_network_observation_id = character(), stream_network_segment_id = character(),
      downstream_segment_id = character(), node_id = character()
    )
  )
}
