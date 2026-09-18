#' Order reference flowlines for upstream or downstream browsing
#'
#' Uses sfnetworks endpoint connectivity and igraph depth-first traversal.
#' Intended for NLDI/NHDPlus reference lines digitized downstream, not raw terrain
#' extraction. No snapping, line reversal, repair or scientific acceptance.
#' @param lines CRS-defined sf LINESTRING reference features.
#' @param comid Identifier of the navigation origin, present in lines. NULL orders
#'   the whole directed network topologically in the requested direction.
#' @param direction "upstream" or "downstream", away from the origin.
#' @param id_column Column containing unique source identifiers.
#' @return Data frame in browsing order: source_row, source_id, navigation_order,
#'   order_status. Unreachable/unsupported features remain explicitly unordered.
#' @export
order_drainage_flowlines <- function(lines, comid = NULL, direction = "upstream",
                                    id_column = "nhdplus_comid") {
  direction <- .fg_choice(direction,c("upstream","downstream"),"direction")
  if (!inherits(lines,"sf") || is.na(sf::st_crs(lines)) || !id_column %in% names(lines))
    .fg_abort("Supply CRS-defined reference lines with a source identifier column.")
  ids <- as.character(lines[[id_column]])
  out <- data.frame(source_row=seq_len(nrow(lines)),source_id=ids,
    navigation_order=rep(NA_integer_,nrow(lines)),order_status=rep("unresolved",nrow(lines)))
  base_order <- order(ids,seq_along(ids),na.last=TRUE)
  if (!nrow(lines) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      (!is.null(comid) && (length(comid)!=1L || is.na(comid) || !comid %in% ids)) ||
      !all(sf::st_geometry_type(lines)=="LINESTRING") ||
      any(sf::st_is_empty(lines)) || !all(sf::st_is_valid(lines) %in% TRUE))
    return(out[base_order,])
  # Sorting input IDs makes branch tie-breaking independent of service row order.
  shape <- sf::st_sf(source_row=base_order,geometry=sf::st_geometry(lines)[base_order])
  net <- tryCatch(sfnetworks::as_sfnetwork(shape,directed=TRUE),error=function(e) NULL)
  if (is.null(net) || !igraph::is_dag(net)) return(out[base_order,])
  edges <- sf::st_drop_geometry(sf::st_as_sf(net,"edges"))
  if (is.null(comid)) {
    # Reverse topological order guarantees every downstream edge precedes its
    # upstream neighbours, including at branches. Unrelated branches have no
    # unique hydrologic order; sorted source IDs make their tie-break stable.
    nodes <- as.integer(igraph::topo_sort(net,
      mode = if (direction == "upstream") "in" else "out"))
    endpoint <- if (direction == "upstream") edges$to else edges$from
    other <- if (direction == "upstream") edges$from else edges$to
    rows <- edges$source_row[order(match(endpoint,nodes),match(other,nodes))]
    out$order_status <- "ordered"
    out$navigation_order[rows] <- seq_along(rows)
    return(out[rows,])
  }
  anchor <- match(match(as.character(comid),ids),edges$source_row)
  # Start beyond the anchor in the requested direction. Starting on its other
  # end would incorrectly include sibling branches at the opposite junction.
  root <- if (direction=="upstream") edges$from[anchor] else edges$to[anchor]
  visit <- as.integer(igraph::dfs(net,root=root,
    mode=if (direction=="upstream") "in" else "out",unreachable=FALSE)$order)
  visit <- visit[!is.na(visit)]
  rank <- pmax(match(edges$from,visit),match(edges$to,visit))
  connected <- which(!is.na(rank))
  walked <- connected[order(rank[connected],ids[edges$source_row[connected]])]
  # The clicked segment leads its own list even at an origin junction.
  walked <- c(anchor,setdiff(walked,anchor))
  rows <- edges$source_row[walked]
  out$order_status <- "unreached"
  out$order_status[rows] <- "ordered"
  out$navigation_order[rows] <- seq_along(rows)
  out[c(rows,setdiff(base_order,rows)),]
}
