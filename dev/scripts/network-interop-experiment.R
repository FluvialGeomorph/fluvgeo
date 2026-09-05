# Standalone experiment, excluded from the R package by ^dev$ in .Rbuildignore.
# Run from fluvgeo: Rscript --vanilla dev/scripts/network-interop-experiment.R
# Reads retained fixtures; modifies only in-memory copies. No dependency changes.
# Mainstem orientation uses an arbitrary terminal, NOT inferred water direction.

required <- c("sf", "sfnetworks", "tidygraph", "igraph", "hydroloom")
stopifnot(all(vapply(required, requireNamespace, logical(1), quietly = TRUE)))
print(setNames(vapply(required, function(p) as.character(utils::packageVersion(p)),
                     character(1)), required))

checks <- 0L
check <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
  checks <<- checks + 1L
}
edges_sf <- function(net) sf::st_as_sf(net, active = "edges")
nodes_sf <- function(net) sf::st_as_sf(net, active = "nodes")
xy_key <- function(xy) apply(xy[, 1:2, drop = FALSE], 1, function(z) {
  paste(sprintf("%.17g", z), collapse = ",")
})
pair_key <- function(from, to) paste(from, to, sep = " -> ")

# The original pruned mainstem has mixed coordinate order and no confirmed
# outlet in this experiment. Give its consolidated copy a coherent ordering
# toward one arbitrary terminal solely to exercise the adapter.
orient_experimental_chain <- function(net) {
  degree <- igraph::degree(net)
  stopifnot(sum(degree == 1L) == 2L, all(degree <= 2L),
            igraph::components(net)$no == 1L)
  outlet <- which(degree == 1L)[1L]
  distance <- as.numeric(igraph::distances(net, to = outlet, weights = NA))
  nodes <- xy_key(sf::st_coordinates(nodes_sf(net)))
  edges <- edges_sf(net)
  start <- match(xy_key(sf::st_coordinates(hydroloom::get_node(edges, "start"))), nodes)
  end <- match(xy_key(sf::st_coordinates(hydroloom::get_node(edges, "end"))), nodes)
  reverse <- distance[start] < distance[end]
  sf::st_geometry(edges)[reverse] <- sf::st_reverse(sf::st_geometry(edges)[reverse])
  # These are old graph indices, not feature attributes for the new graph.
  edges$from <- edges$to <- NULL
  sfnetworks::as_sfnetwork(edges, directed = TRUE)
}

exercise <- function(source, label, directed, protect = NULL, source_flow = NULL) {
  before <- source
  net <- sfnetworks::as_sfnetwork(source, directed = directed)
  clean <- tidygraph::convert(
    net, sfnetworks::to_spatial_smooth,
    protect = protect, require_equal = "boundary_class", store_original_data = TRUE,
    summarise_attributes = list(boundary_class = "first", "ignore")
  )
  edges <- edges_sf(clean)
  members <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
    data.frame(link_id = paste0("L", i), source_id = edges$.orig_data[[i]]$source_id)
  }))
  check(nrow(members) == nrow(source) && !anyDuplicated(members$source_id) &&
          setequal(members$source_id, source$source_id), paste(label, "complete lineage"))
  check(all(vapply(edges$.orig_data, function(s) {
    length(unique(s$boundary_class)) == 1L
  }, logical(1))), paste(label, "attribute boundaries"))
  check(identical(edges$boundary_class, vapply(edges$.orig_data, function(s) {
    as.character(s$boundary_class[1L])
  }, character(1))), paste(label, "retained classification values"))
  check(all(vapply(edges$.orig_data, function(s) {
    original <- source[match(s$source_id, source$source_id), ]
    identical(sf::st_as_binary(sf::st_geometry(s)),
              sf::st_as_binary(sf::st_geometry(original)))
  }, logical(1))), paste(label, "original coordinate sequences in lineage"))
  check(sf::st_crs(source) == sf::st_crs(edges), paste(label, "CRS"))
  length_delta <- abs(as.numeric(sum(sf::st_length(source)) - sum(sf::st_length(edges))))
  check(length_delta < 1e-7, paste(label, "total length"))
  check(isTRUE(sf::st_equals(sf::st_union(source), sf::st_union(edges), sparse = FALSE)[1, 1]),
        paste(label, "geometric coverage"))
  check(all(sf::st_is_valid(edges)) && !any(sf::st_is_empty(edges)), paste(label, "geometry validity"))
  check(igraph::components(net, mode = "weak")$no ==
          igraph::components(clean, mode = "weak")$no, paste(label, "components"))
  # Compare directed junction/terminal degrees when direction is known; use
  # undirected junction/terminal degree signatures for the mainstem.
  signature <- function(g) {
    degree <- igraph::degree(g)
    keep <- degree != 2L
    if (directed) keep <- !(igraph::degree(g, mode = "in") == 1L &
                            igraph::degree(g, mode = "out") == 1L)
    xy <- xy_key(sf::st_coordinates(nodes_sf(g)))
    if (directed) sort(paste(xy[keep], igraph::degree(g, mode = "in")[keep],
                            igraph::degree(g, mode = "out")[keep])) else
      sort(paste(xy[keep], degree[keep]))
  }
  check(identical(signature(net), signature(clean)), paste(label, "junctions and terminals"))
  if (!is.null(protect)) {
    protected_xy <- xy_key(sf::st_coordinates(nodes_sf(net)[protect, ]))
    check(all(protected_xy %in% xy_key(sf::st_coordinates(nodes_sf(clean)))),
          paste(label, "protected node"))
  }
  again <- tidygraph::convert(clean, sfnetworks::to_spatial_smooth,
                             protect = if (is.null(protect)) NULL else nodes_sf(net)[protect, ],
                             require_equal = "boundary_class",
                             summarise_attributes = list(boundary_class = "first", "ignore"))
  check(igraph::ecount(again) == igraph::ecount(clean), paste(label, "repeat-run edge count"))

  # Identity resides on features, never in sfnetworks' positional from/to keys.
  # Preserve these experiment IDs across the optional direction adapter.
  clean <- tidygraph::activate(clean, "edges")
  clean <- dplyr::mutate(clean, experiment_id = paste0("L", seq_len(dplyr::n())))
  flow_net <- if (directed) clean else orient_experimental_chain(clean)
  e <- edges_sf(flow_net)
  h <- sf::st_sf(id = e$experiment_id,
                fromnode = paste0("N", e$from), tonode = paste0("N", e$to),
                geometry = sf::st_geometry(e))
  check(inherits(hydroloom::hy(h), "hy_node"), paste(label, "hydroloom node representation"))
  # FGDB stores upstream coordinate order. Reverse a COPY for storage and
  # reverse it back for downstream-oriented computation; attributes stay put.
  storage <- h
  sf::st_geometry(storage) <- sf::st_reverse(sf::st_geometry(h))
  restored <- storage
  sf::st_geometry(restored) <- sf::st_reverse(sf::st_geometry(storage))
  check(identical(sf::st_as_binary(sf::st_geometry(h)),
                  sf::st_as_binary(sf::st_geometry(restored))), paste(label, "direction round trip"))
  check(identical(sf::st_drop_geometry(h), sf::st_drop_geometry(storage)),
        paste(label, "identity through direction adapter"))
  topology <- hydroloom::make_attribute_topology(restored, min_distance = 0)
  actual <- pair_key(topology$id[topology$toid %in% topology$id],
                     topology$toid[topology$toid %in% topology$id])
  # Independent expected relationships from explicit graph endpoint indices.
  expected <- merge(sf::st_drop_geometry(h)[c("id", "tonode")],
                    sf::st_drop_geometry(h)[c("id", "fromnode")],
                    by.x = "tonode", by.y = "fromnode")
  check(setequal(actual, pair_key(expected$id.x, expected$id.y)),
        paste(label, "rebuilt connections agree with spatial graph"))
  if (!is.null(source_flow)) {
    # Independently compare with the fixture's original hydrologic relationships,
    # mapped through the many-source-to-one-link lineage and excluding internal joins.
    map <- setNames(members$link_id, members$source_id)
    a <- unname(map[as.character(source_flow$id)])
    b <- unname(map[as.character(source_flow$toid)])
    external <- !is.na(b) & a != b
    check(setequal(actual, pair_key(a[external], b[external])),
          paste(label, "all original inter-link relationships"))
    check(inherits(hydroloom::hy(topology), "hy_flownetwork"),
          paste(label, "non-dendritic connections retained"))
  }
  sorted <- hydroloom::sort_network(topology)
  check(setequal(sorted$id, h$id), paste(label, "hydroloom sorted feature coverage"))
  # A relationship table may repeat ids at divergences; do not join its rows
  # back as if it were a unique-feature table.
  starts <- h$id[!h$fromnode %in% h$tonode]
  reached <- unique(unlist(hydroloom::navigate_network_dfs(topology, starts, direction = "down"),
                           use.names = FALSE))
  check(setequal(reached, h$id), paste(label, "hydroloom downstream navigation coverage"))
  check(identical(source, before), paste(label, "unchanged input"))
  data.frame(case = label, source_pieces = nrow(source), logical_links = nrow(h),
             retained_sources = nrow(members), connections = length(unique(actual)),
             outlets = sum(!topology$toid %in% topology$id),
             length_difference_m = length_delta)
}

mainstem <- suppressWarnings(sf::st_read(
  "../fluvgeodata/inst/extdata/testing_data.gdb", layer = "stream_network", quiet = TRUE
))
mainstem <- suppressWarnings(sf::st_cast(mainstem, "LINESTRING"))
mainstem <- sf::st_sf(source_id = as.character(mainstem$arcid),
                     boundary_class = mainstem$ReachName, geometry = sf::st_geometry(mainstem))
main_net <- sfnetworks::as_sfnetwork(mainstem, directed = FALSE)
# Explicit experimental boundary at the end of the equal-elevation source 1126.
# This is NOT a claim that an actual Reach boundary exists there.
endpoint <- hydroloom::get_node(mainstem[mainstem$source_id == "1126", ], "end")
protected <- which(lengths(sf::st_equals(nodes_sf(main_net), endpoint)) > 0L)
check(length(protected) == 1L, "unique experimental boundary")

new_hope <- hydroloom::hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
check(all(lengths(sf::st_geometry(new_hope)) == 1L), "New Hope single-part geometries")
source_flow <- hydroloom::to_flownetwork(new_hope)
new_hope <- suppressWarnings(sf::st_cast(new_hope, "LINESTRING"))
branched <- sf::st_sf(source_id = as.character(new_hope$id),
                     boundary_class = new_hope$feature_type, geometry = sf::st_geometry(new_hope))
results <- rbind(
  exercise(mainstem, "Sinsinawa", FALSE),
  exercise(mainstem, "Sinsinawa protected", FALSE, protect = protected),
  exercise(branched, "New Hope branches/diversions", TRUE, source_flow = source_flow)
)
print(results, row.names = FALSE)
cat("PASS:", checks, "checks. No production or source-data writes.\n")
