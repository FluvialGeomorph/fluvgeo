#' Select a terrain-derived Stream mainstem
#'
#' Enumerates every head-to-outlet path in a directed synthetic Stream Network,
#' compares each complete route with the retained reference hydrography, and
#' selects the longest route among those with the best full-route reference
#' agreement. Reference geometry identifies the Stream previously chosen by the
#' analyst; output coordinates come only from the terrain-derived network.
#'
#' @param stream_network Projected `sf` LINESTRING features produced by
#'   [extract_synthetic_stream_network()]. Required fields are
#'   `stream_line_id`, `upstream_cell`, and `downstream_cell`.
#' @param reference_lines CRS-defined `sf` line features representing the saved
#'   NHDPlusV2 Stream selection. Lines may be split into Reach source pieces.
#' @return A list containing the downstream-to-upstream one-feature `flowline`,
#'   upstream-to-downstream `selected_segments`, and one `candidates` feature per
#'   complete head-to-outlet route with length, discrete Hausdorff distance,
#'   rank, and selection evidence.
#' @details The function validates a one-outlet acyclic directed tree and exact
#'   segment connectivity. It does not snap, smooth, simplify, or modify input
#'   coordinates. Discrete Hausdorff distance is calculated by GEOS in the
#'   network CRS and reported in metres. A stable upstream-cell tie-break makes
#'   equivalent results independent of input row order.
#' @export
select_stream_mainstem <- function(stream_network, reference_lines) {
  required <- c("stream_line_id", "upstream_cell", "downstream_cell")
  if (!inherits(stream_network, "sf") || !nrow(stream_network) ||
      is.na(sf::st_crs(stream_network)) || sf::st_is_longlat(stream_network) ||
      !all(required %in% names(stream_network)) ||
      !all(sf::st_geometry_type(stream_network) == "LINESTRING") ||
      any(sf::st_is_empty(stream_network)) ||
      any(sf::st_is_valid(stream_network) %in% FALSE))
    stop("Supply a nonempty projected synthetic Stream Network of valid LINESTRING features.")
  if (anyNA(stream_network$stream_line_id) ||
      any(!nzchar(as.character(stream_network$stream_line_id))) ||
      anyDuplicated(stream_network$stream_line_id) ||
      anyNA(stream_network$upstream_cell) ||
      anyNA(stream_network$downstream_cell) ||
      anyDuplicated(stream_network$upstream_cell))
    stop("Stream Network identifiers and directed cell fields must be complete and unique.")
  if (!inherits(reference_lines, "sf") || !nrow(reference_lines) ||
      is.na(sf::st_crs(reference_lines)) ||
      !all(sf::st_geometry_type(reference_lines) %in% c("LINESTRING", "MULTILINESTRING")) ||
      any(sf::st_is_empty(reference_lines)) ||
      any(sf::st_is_valid(reference_lines) %in% FALSE))
    stop("Supply nonempty, valid, CRS-defined reference linework.")

  network <- stream_network
  network$.input_order <- seq_len(nrow(network))
  network <- network[order(as.numeric(network$upstream_cell),
    as.character(network$stream_line_id), network$.input_order), ]
  next_row <- match(network$downstream_cell, network$upstream_cell)
  if (sum(is.na(next_row)) != 1L)
    stop("The synthetic Stream Network must have exactly one observed outlet.")
  incoming <- tabulate(next_row[!is.na(next_row)], nbins = nrow(network))
  heads <- which(incoming == 0L)
  if (!length(heads)) stop("The synthetic Stream Network has no observed head.")

  paths <- lapply(heads, function(head) {
    path <- integer(); current <- head
    while (!is.na(current)) {
      if (current %in% path)
        stop("The synthetic Stream Network contains a directed cycle.")
      path <- c(path, current)
      current <- next_row[current]
    }
    path
  })
  if (!setequal(unlist(paths, use.names = FALSE), seq_len(nrow(network))))
    stop("The synthetic Stream Network contains a cycle or a component that does not reach the outlet.")

  coordinates <- lapply(sf::st_geometry(network), function(g)
    unname(sf::st_coordinates(g)[, c("X", "Y"), drop = FALSE]))
  coordinate_scale <- max(1, abs(unlist(coordinates, use.names = FALSE)))
  connection_tolerance <- coordinate_scale * 1e-12
  route_geometry <- lapply(paths, function(path) {
    xy <- coordinates[[path[1]]]
    if (length(path) > 1L) for (row in path[-1]) {
      next_xy <- coordinates[[row]]
      gap <- sqrt(sum((xy[nrow(xy), ] - next_xy[1, ])^2))
      if (!is.finite(gap) || gap > connection_tolerance)
        stop("Directed Stream Network cell links do not have coincident geometry endpoints.")
      xy <- rbind(xy, next_xy[-1, , drop = FALSE])
    }
    sf::st_linestring(xy)
  })
  route_sfc <- sf::st_sfc(route_geometry, crs = sf::st_crs(network))
  route_length <- as.numeric(units::set_units(sf::st_length(route_sfc), "m"))

  reference <- sf::st_transform(reference_lines, sf::st_crs(network))
  reference_geometry <- sf::st_union(sf::st_geometry(reference))
  candidates <- sf::st_sf(
    route_id = sprintf("P%05d", seq_along(paths)),
    head_cell = as.numeric(network$upstream_cell[heads]),
    segment_count = lengths(paths),
    length_m = route_length,
    geometry = route_sfc
  )
  distance <- sf::st_distance(candidates, reference_geometry, which = "Hausdorff")
  candidates$reference_hausdorff_m <- as.numeric(units::set_units(distance[, 1], "m"))
  if (any(!is.finite(candidates$reference_hausdorff_m)))
    stop("Reference agreement could not be calculated for every complete route.")

  best_distance <- min(candidates$reference_hausdorff_m)
  equivalent <- abs(candidates$reference_hausdorff_m - best_distance) <=
    max(1, best_distance) * sqrt(.Machine$double.eps)
  eligible <- which(equivalent)
  selected <- eligible[order(-candidates$length_m[eligible],
    candidates$head_cell[eligible], candidates$route_id[eligible])][1]
  ranking <- order(candidates$reference_hausdorff_m, -candidates$length_m,
    candidates$head_cell, candidates$route_id)
  candidates$selection_rank <- match(seq_len(nrow(candidates)), ranking)
  candidates$reference_best <- equivalent
  candidates$selected <- seq_len(nrow(candidates)) == selected

  path <- paths[[selected]]
  selected_segments <- network[path, setdiff(names(network), ".input_order"), drop = FALSE]
  selected_segments$path_order_upstream <- seq_len(nrow(selected_segments))
  selected_segments$path_order_flowline <- rev(seq_len(nrow(selected_segments)))
  upstream_xy <- sf::st_coordinates(candidates[selected, ])[, c("X", "Y"), drop = FALSE]
  flowline_geometry <- sf::st_linestring(unname(upstream_xy[nrow(upstream_xy):1, , drop = FALSE]))
  ordered_distance <- sort(candidates$reference_hausdorff_m)
  margin <- if (length(ordered_distance) > 1L)
    ordered_distance[2] - ordered_distance[1] else NA_real_
  flowline <- sf::st_sf(
    route_id = candidates$route_id[selected],
    selection_method = "minimum full-route discrete Hausdorff distance to saved reference; longest route among equivalent best matches; stable upstream-cell tie-break",
    selected_head_cell = candidates$head_cell[selected],
    source_segment_count = nrow(selected_segments),
    length_m = candidates$length_m[selected],
    reference_hausdorff_m = candidates$reference_hausdorff_m[selected],
    reference_margin_m = margin,
    geometry = sf::st_sfc(flowline_geometry, crs = sf::st_crs(network))
  )
  list(flowline = flowline, selected_segments = selected_segments,
    candidates = candidates)
}
