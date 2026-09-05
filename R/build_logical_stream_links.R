#' Build logical links from raw stream line segments
#'
#' Joins exact-endpoint, degree-two continuations using sfnetworks. This is
#' topological concatenation, not cartographic smoothing or a length threshold.
#' Coordinate order is not interpreted as flow direction. Junctions, terminals,
#' attribute boundaries, and explicitly protected endpoints remain boundaries.
#' All edges in undirected cycles, self-intersections, overlaps, duplicates,
#' interior crossings, and endpoint near misses are retained without merging.
#' This conservative operation does not snap, split, repair defects, or establish
#' hydrologic direction. Missing boundary values may join other missing values,
#' but never known values. Undeclared semantic boundaries cannot be inferred.
#'
#' @param lines Nonempty projected XY LINESTRING sf object. Normalize multipart
#'   features before calling this function.
#' @param boundary_fields Character names of atomic attribute columns whose
#'   values must agree for a join. Only these attributes are retained on links.
#' @param protected_nodes Optional sf or sfc POINTs in the same CRS, each exactly
#'   coincident with an existing endpoint. No nearest-node matching is performed.
#' @param tolerance Nonnegative endpoint near-miss distance in CRS units. It
#'   protects implicated edges from merging; it is never a snapping distance.
#'
#' @return A list with `links` (sf, one row per logical link, `link_row` plus
#'   boundary attributes) and `membership` (tibble with `link_row`, `input_row`).
#'   Every input row occurs exactly once in membership. Row keys are local to
#'   this result, not persistent identities. Singleton coordinate order and
#'   input objects are unchanged; merged coordinate order is arbitrary.
#' @export
build_logical_stream_links <- function(
    lines, boundary_fields = character(), protected_nodes = NULL, tolerance = 0) {
  if (!inherits(lines, "sf") || !nrow(lines) ||
      is.na(sf::st_crs(lines)) || isTRUE(sf::st_is_longlat(lines))) {
    .fg_abort("`lines` must be nonempty projected sf linework with a CRS.")
  }
  geometry <- sf::st_geometry(lines)
  if (any(sf::st_geometry_type(lines) != "LINESTRING") ||
      any(vapply(geometry, function(g) !inherits(g, "XY"), logical(1))) ||
      any(sf::st_is_empty(lines)) || anyNA(sf::st_is_valid(lines)) ||
      any(!sf::st_is_valid(lines)) || any(as.numeric(sf::st_length(lines)) <= 0)) {
    .fg_abort("`lines` must contain valid, positive-length XY LINESTRINGs.")
  }
  if (!is.character(boundary_fields) || anyNA(boundary_fields) ||
      anyDuplicated(boundary_fields) ||
      any(!boundary_fields %in% names(sf::st_drop_geometry(lines))) ||
      any(boundary_fields %in% c("link_row", ".input_row", ".boundary")) ||
      any(!vapply(sf::st_drop_geometry(lines)[boundary_fields], is.atomic, logical(1)))) {
    .fg_abort("`boundary_fields` must name distinct atomic attributes, not reserved fields.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0) {
    .fg_abort("`tolerance` must be one finite nonnegative distance in CRS units.")
  }
  # Integer classes avoid NA equality ambiguities and delimiter collisions.
  classes <- lapply(sf::st_drop_geometry(lines)[boundary_fields], function(x) {
    match(x, unique(x))
  })
  boundary <- if (length(classes)) do.call(paste, c(classes, sep = ":")) else
    rep("all", nrow(lines))
  input <- sf::st_sf(.input_row = seq_len(nrow(lines)), .boundary = boundary,
                     geometry = geometry)
  net <- sfnetworks::as_sfnetwork(input, directed = FALSE)
  nodes <- sf::st_as_sf(net, active = "nodes")
  edges <- sf::st_as_sf(net, active = "edges")
  protect <- integer()
  if (!is.null(protected_nodes)) {
    if (!inherits(protected_nodes, c("sf", "sfc")) ||
        is.na(sf::st_crs(protected_nodes)) ||
        sf::st_crs(protected_nodes) != sf::st_crs(lines) ||
        any(sf::st_geometry_type(protected_nodes) != "POINT") ||
        any(sf::st_is_empty(protected_nodes))) {
      .fg_abort("`protected_nodes` must be nonempty POINT geometries in the linework CRS.")
    }
    matches <- sf::st_equals(protected_nodes, nodes)
    if (any(lengths(matches) != 1L)) {
      .fg_abort("Every protected node must exactly match an existing endpoint.")
    }
    protect <- unlist(matches, use.names = FALSE)
  }
  # A degree-two contraction must not erase the evidence needed to assess raw
  # defects. Preserve both endpoints of every implicated edge. Cyclic edges
  # are non-bridges; retaining them also prevents contracting a ring to a loop.
  unsafe <- union(which(!sf::st_is_simple(geometry)),
                  setdiff(seq_len(nrow(edges)), as.integer(igraph::bridges(net))))
  nearby <- sf::st_is_within_distance(geometry, dist = tolerance)
  endpoints <- lapply(geometry, function(g) sf::st_sfc(
    sf::st_point(g[1L, ]), sf::st_point(g[nrow(g), ]), crs = sf::st_crs(lines)))
  for (i in seq_len(nrow(lines))) {
    for (j in nearby[[i]][nearby[[i]] > i]) {
      relation <- sf::st_relate(geometry[i], geometry[j])[1L, 1L]
      distances <- as.numeric(sf::st_distance(endpoints[[i]], endpoints[[j]]))
      if (any(substring(relation, c(1L, 2L, 4L), c(1L, 2L, 4L)) != "F") ||
          any(distances > 0 & distances <= tolerance)) {
        unsafe <- union(unsafe, c(i, j))
      }
    }
  }
  protect <- unique(c(protect, edges$from[unsafe], edges$to[unsafe]))
  clean <- tidygraph::convert(
    net, sfnetworks::to_spatial_smooth, protect = protect,
    require_equal = ".boundary", store_original_data = TRUE,
    summarise_attributes = list("ignore")
  )
  output <- sf::st_as_sf(clean, active = "edges")
  # sfnetworks returns the input graph unchanged when no contraction is possible,
  # in which case it does not create .orig_data at all.
  members <- if (".orig_data" %in% names(output)) {
    lapply(output$.orig_data, function(x) sort(x$.input_row))
  } else lapply(output$.input_row, as.integer)
  # Stable local ordering, independent of graph renumbering.
  order <- order(vapply(members, min, integer(1)))
  members <- members[order]
  output <- output[order, ]
  membership <- tibble::tibble(
    link_row = rep(seq_along(members), lengths(members)),
    input_row = as.integer(unlist(members, use.names = FALSE))
  )
  if (!identical(sort(membership$input_row), seq_len(nrow(lines)))) {
    .fg_abort("Logical-link construction failed to preserve complete source membership.")
  }
  singleton <- which(lengths(members) == 1L)
  sf::st_geometry(output)[singleton] <- geometry[vapply(members[singleton], `[`, integer(1), 1L)]
  first <- vapply(members, `[`, integer(1), 1L)
  links <- sf::st_sf(
    dplyr::bind_cols(tibble::tibble(link_row = seq_along(members)),
                    sf::st_drop_geometry(lines)[first, boundary_fields, drop = FALSE]),
    geometry = sf::st_geometry(output)
  )
  list(links = links, membership = membership)
}
