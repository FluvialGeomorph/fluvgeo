#' Divide a selected Stream Flowline among its saved Reaches
#'
#' Transfers ordered Reach transitions from the retained Stream reference to a
#' terrain-derived Flowline, then prepares one continuous Flowline per Reach.
#'
#' @param raw_flowline One projected, downstream-to-upstream \code{sf} LINESTRING
#'   selected from the synthetic Stream Network.
#' @param smoothed_flowline A smoothed version of \code{raw_flowline} with the same
#'   fixed endpoints.
#' @param reference_lines Ordered downstream-to-upstream retained Stream
#'   reference lines with a unique \code{selection_id} field.
#' @param reach_mappings Data frame linking every retained \code{selection_id} to one
#'   saved \code{reach_id}.
#' @param reaches Saved Reach features or data frame with unique \code{reach_id} and
#'   \code{reach_name} fields.
#' @param dem A \code{terra::SpatRaster} in the Flowline CRS. It is supplied to
#'   \code{flowline()} for contract compatibility; saved topology remains the
#'   direction authority.
#' @param connectivity_tolerance Maximum permitted gap between consecutive
#'   reference lines, in reference CRS map units. Defaults to 0.01.
#' @return A list containing \code{flowlines} (one downstream-to-upstream feature per
#'   Reach with legacy \code{ReachName}, \code{from_measure}, and \code{to_measure}
#'   fields; measures are continuous kilometres from the selected Stream outlet),
#'   \code{boundaries} (transferred Reach boundary points and projection evidence),
#'   and \code{reach_order}.
#' @details Each Reach must occupy one contiguous block in the ordered retained
#'   Stream reference. Every reference line must be assigned exactly once. Reach
#'   boundaries are the shared or near-coincident endpoints between consecutive
#'   reference blocks. They are projected first to the raw terrain path, then to
#'   the smoothed path. Reversed, duplicated, gapped or collapsed transitions
#'   fail closed. Reach polygon edges are not used.
#' @export
derive_reach_flowlines <- function(raw_flowline, smoothed_flowline,
                                   reference_lines, reach_mappings, reaches,
                                   dem, connectivity_tolerance = 0.01) {
  check_line <- function(x, label) {
    if (!inherits(x, "sf") || nrow(x) != 1L || is.na(sf::st_crs(x)) ||
        sf::st_is_longlat(x) || sf::st_is_empty(x) || !sf::st_is_valid(x) ||
        !sf::st_is_simple(x) ||
        !identical(as.character(sf::st_geometry_type(x)), "LINESTRING"))
      stop(label, " must be one valid, simple, projected LINESTRING.")
  }
  check_line(raw_flowline, "raw_flowline")
  check_line(smoothed_flowline, "smoothed_flowline")
  if (sf::st_crs(raw_flowline) != sf::st_crs(smoothed_flowline) ||
      sf::st_crs(raw_flowline) != sf::st_crs(dem))
    stop("Flowlines and dem must use the same CRS.")
  raw_xy <- sf::st_coordinates(raw_flowline)[, c("X", "Y"), drop = FALSE]
  smooth_xy <- sf::st_coordinates(smoothed_flowline)[, c("X", "Y"), drop = FALSE]
  if (!isTRUE(all.equal(raw_xy[c(1L, nrow(raw_xy)), , drop = FALSE],
      smooth_xy[c(1L, nrow(smooth_xy)), , drop = FALSE],
      tolerance = sqrt(.Machine$double.eps), check.attributes = FALSE)))
    stop("raw_flowline and smoothed_flowline must have identical endpoints.")
  if (!inherits(reference_lines, "sf") || nrow(reference_lines) < 1L ||
      is.na(sf::st_crs(reference_lines)) || sf::st_is_longlat(reference_lines) ||
      sf::st_crs(reference_lines) != sf::st_crs(raw_flowline) ||
      !"selection_id" %in% names(reference_lines) ||
      anyNA(reference_lines$selection_id) || anyDuplicated(reference_lines$selection_id))
    stop("reference_lines must be ordered projected Stream lines with unique selection_id values.")
  required_mapping <- c("selection_id", "reach_id")
  if (!is.data.frame(reach_mappings) ||
      !all(required_mapping %in% names(reach_mappings)) ||
      anyNA(reach_mappings[required_mapping]) ||
      anyDuplicated(reach_mappings$selection_id) ||
      !setequal(reach_mappings$selection_id, reference_lines$selection_id))
    stop("Every retained Stream reference line must map to exactly one Reach.")
  if (!is.data.frame(reaches) || !all(c("reach_id", "reach_name") %in% names(reaches)) ||
      anyNA(reaches[c("reach_id", "reach_name")]) || anyDuplicated(reaches$reach_id))
    stop("reaches must contain unique reach_id and reach_name values.")
  if (!is.numeric(connectivity_tolerance) || length(connectivity_tolerance) != 1L ||
      !is.finite(connectivity_tolerance) || connectivity_tolerance < 0)
    stop("connectivity_tolerance must be one non-negative finite number.")

  reach_id <- reach_mappings$reach_id[match(reference_lines$selection_id,
    reach_mappings$selection_id)]
  if (any(!reach_id %in% reaches$reach_id))
    stop("A retained Stream reference line maps to a Reach absent from this revision.")
  blocks <- cumsum(c(TRUE, reach_id[-1L] != reach_id[-length(reach_id)]))
  reach_order <- reach_id[!duplicated(blocks)]
  if (anyDuplicated(reach_order))
    stop("A Reach occupies noncontiguous retained Stream reference blocks.")

  raw_geometry <- sf::st_geometry(raw_flowline)
  smooth_geometry <- sf::st_geometry(smoothed_flowline)
  transitions <- which(reach_id[-length(reach_id)] != reach_id[-1L])
  raw_fraction <- smooth_fraction <- gap <- numeric(length(transitions))
  raw_distance <- numeric(length(transitions))
  boundary_geometry <- vector("list", length(transitions))
  for (k in seq_along(transitions)) {
    i <- transitions[k]
    connector <- sf::st_nearest_points(sf::st_geometry(reference_lines[i, ]),
      sf::st_geometry(reference_lines[i + 1L, ]))
    gap[k] <- as.numeric(sf::st_length(connector))
    if (!is.finite(gap[k]) || gap[k] > connectivity_tolerance)
      stop("Consecutive Reach reference blocks contain a gap larger than connectivity_tolerance.")
    points <- suppressWarnings(sf::st_cast(connector, "POINT"))
    xy <- sf::st_coordinates(points)[, c("X", "Y"), drop = FALSE]
    reference_point <- sf::st_sfc(sf::st_point(colMeans(xy)),
      crs = sf::st_crs(reference_lines))
    located <- .fg_flowline_fraction(raw_geometry, reference_point)
    raw_fraction[k] <- located$fraction
    raw_point <- sf::st_line_interpolate(raw_geometry, raw_fraction[k],
      normalized = TRUE)
    raw_distance[k] <- located$distance
    smooth_fraction[k] <- .fg_flowline_fraction(smooth_geometry,
      raw_point)$fraction
    boundary_geometry[[k]] <- sf::st_line_interpolate(smooth_geometry,
      smooth_fraction[k], normalized = TRUE)[[1]]
  }
  if (length(transitions) &&
      (any(!is.finite(raw_fraction)) || any(!is.finite(smooth_fraction)) ||
       any(raw_fraction <= 0 | raw_fraction >= 1) ||
       any(smooth_fraction <= 0 | smooth_fraction >= 1) ||
       is.unsorted(raw_fraction, strictly = TRUE) ||
       is.unsorted(smooth_fraction, strictly = TRUE)))
    stop("Reach boundary projections are reversed, ambiguous or collapse on the selected Flowline.")

  cuts <- c(0, smooth_fraction, 1)
  rows <- lapply(seq_along(reach_order), function(i) {
    geometry <- lwgeom::st_linesubstring(smooth_geometry, cuts[i], cuts[i + 1L])
    item <- sf::st_sf(geometry = geometry)
    prepared <- flowline(item,
      reaches$reach_name[match(reach_order[i], reaches$reach_id)], dem,
      direction = "preserve")
    prepared$reach_id <- reach_order[i]
    prepared$reach_order <- i
    prepared$length_m <- as.numeric(units::set_units(sf::st_length(prepared), "m"))
    prepared[, c("reach_id", "ReachName", "reach_order", "length_m", "geometry")]
  })
  flowlines <- do.call(rbind, rows)
  if (any(sf::st_is_empty(flowlines)) || any(as.numeric(sf::st_length(flowlines)) <= 0))
    stop("Reach division produced an empty Flowline.")
  if (nrow(flowlines) > 1L) {
    for (i in seq_len(nrow(flowlines) - 1L)) {
      a <- tail(sf::st_coordinates(flowlines[i, ])[, c("X", "Y"), drop = FALSE], 1L)
      b <- head(sf::st_coordinates(flowlines[i + 1L, ])[, c("X", "Y"), drop = FALSE], 1L)
      if (!identical(unname(a), unname(b)))
        stop("Adjacent Reach Flowlines do not share an exact endpoint.")
    }
  }
  flowlines$from_measure <- c(0, head(cumsum(flowlines$length_m / 1000), -1L))
  flowlines$to_measure <- cumsum(flowlines$length_m / 1000)
  geometry_column <- attr(flowlines, "sf_column")
  flowlines <- flowlines[, c("reach_id", "ReachName", "reach_order",
    "from_measure", "to_measure", "length_m", geometry_column)]
  for (i in seq_len(nrow(flowlines)))
    check_flowline(flowlines[i, ], step = "profile_points")
  boundaries <- sf::st_sf(
    downstream_reach_id = if (length(transitions)) reach_id[transitions] else character(),
    upstream_reach_id = if (length(transitions)) reach_id[transitions + 1L] else character(),
    raw_fraction = raw_fraction, smoothed_fraction = smooth_fraction,
    reference_gap = gap, reference_to_raw_distance = raw_distance,
    geometry = sf::st_sfc(boundary_geometry, crs = sf::st_crs(raw_flowline)))
  list(flowlines = flowlines, boundaries = boundaries,
    reach_order = reach_order)
}

.fg_flowline_fraction <- function(line, point) {
  xy <- sf::st_coordinates(line)[, c("X", "Y"), drop = FALSE]
  p <- sf::st_coordinates(point)[1L, c("X", "Y")]
  start <- xy[-nrow(xy), , drop = FALSE]
  vector <- xy[-1L, , drop = FALSE] - start
  squared <- rowSums(vector^2)
  if (any(!is.finite(squared)) || any(squared <= 0))
    stop("Flowline contains a zero-length or invalid segment.")
  t <- pmax(0, pmin(1, rowSums((matrix(p, nrow(start), 2L, byrow = TRUE) -
    start) * vector) / squared))
  projected <- start + vector * t
  distance <- sqrt(rowSums((projected - matrix(p, nrow(start), 2L,
    byrow = TRUE))^2))
  segment_length <- sqrt(squared)
  before <- c(0, cumsum(segment_length))[seq_along(segment_length)]
  along <- before + t * segment_length
  minimum <- min(distance)
  candidates <- along[abs(distance - minimum) <=
    max(sqrt(.Machine$double.eps), minimum * 1e-10)]
  candidates <- unique(round(candidates, 12L))
  if (length(candidates) != 1L)
    stop("A Reach boundary has more than one equally near position on the Flowline.")
  list(fraction = candidates / sum(segment_length), distance = minimum)
}
