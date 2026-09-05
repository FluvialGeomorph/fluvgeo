# Shared geometry assessment; node identity/connectivity remains in the adapter.
.fg_stream_geometry_findings <- function(geometry, tolerance) {
  n <- length(geometry)
  findings <- list()
  add_finding <- function(i, code, message, j = NA_integer_) {
    findings[[length(findings) + 1L]] <<- list(i = i, j = j, code = code, message = message)
  }
  simple <- sf::st_is_simple(geometry)
  endpoints <- sf::st_sfc(lapply(geometry, function(line) {
    sf::st_multipoint(unclass(line)[c(1L, nrow(line)), 1:2, drop = FALSE])
  }), crs = sf::st_crs(geometry))
  for (i in seq_len(n)) {
    if (!simple[i]) {
      add_finding(i, "SELF_INTERSECTION", "Inspect this self-intersecting segment.")
    }
    if (all(endpoints[[i]][1L, ] == endpoints[[i]][2L, ])) {
      add_finding(i, "CLOSED_SEGMENT", "Inspect this segment whose endpoints coincide.")
    }
  }

  # Sparse candidate pairs avoid constructing a dense all-pairs distance matrix.
  nearby <- sf::st_is_within_distance(geometry, dist = tolerance)
  for (i in seq_len(n)) {
    for (j in nearby[[i]][nearby[[i]] > i]) {
      if (length(sf::st_equals(geometry[i], geometry[j])[[1L]])) {
        add_finding(i, "DUPLICATE_GEOMETRY", paste(
          "These segments cover the same geometry; review their source lineage",
          "before deciding which geometry to retain."
        ), j)
        next
      }
      relation <- sf::st_relate(geometry[i], geometry[j])[1L, 1L]
      if (any(substring(relation, c(1L, 2L, 4L), c(1L, 2L, 4L)) != "F")) {
        add_finding(i, "INTERIOR_INTERSECTION", paste(
          "The lines intersect at an interior or overlap. Determine whether",
          "a confluence split, overlap repair, or qualified crossing is needed."
        ), j)
      }
      # Measure all four endpoint pairs, including cases where a different pair
      # coincides exactly. Never snap endpoints automatically.
      a <- sf::st_cast(endpoints[i], "POINT")
      b <- sf::st_cast(endpoints[j], "POINT")
      distances <- as.numeric(sf::st_distance(a, b))
      if (any(distances > 0 & distances <= tolerance)) {
        add_finding(i, "ENDPOINT_NEAR_MISS", paste(
          "Noncoincident endpoints are within the observation tolerance.",
          "Inspect both segments before choosing a snap target or retaining a gap."
        ), j)
      }
    }
  }
  findings
}
