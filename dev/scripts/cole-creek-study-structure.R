# Project-specific fixture interpretation, not a general HUC-based hierarchy rule.
# Caller supplies provisional demonstration IDs. Archive layers are read only.
cole_creek_study_structure <- function(gdb, reach_flowline, study_area_id, reach_id) {
  huc <- sf::st_read(gdb, layer = "Papillion_HUC12", quiet = TRUE)
  stopifnot(nrow(huc) == 7L, !anyDuplicated(huc$HUC12), !anyDuplicated(huc$Name),
    all(sf::st_is_valid(huc)), all(!sf::st_is_empty(huc)))
  # Explicit analytical CRS matches the existing Cole Creek reach fixture.
  # Stream polygons retain their source CRS/coordinates; only the union is derived.
  projected <- sf::st_transform(huc, 26914)
  boundary <- sf::st_union(sf::st_geometry(projected))
  source_area <- as.numeric(sum(sf::st_area(projected)))
  union_area <- as.numeric(sf::st_area(boundary))
  area_difference <- source_area - union_area
  # Numeric overlay tolerance, not permission to repair/snap source boundaries.
  area_tolerance <- max(0.001, source_area * 1e-12)
  stopifnot(all(sf::st_is_valid(boundary)), length(sf::st_cast(boundary, "POLYGON")) == 1L,
    abs(area_difference) < area_tolerance,
    all(lengths(sf::st_covered_by(projected, boundary)) == 1L))
  parent <- sf::st_covered_by(sf::st_transform(reach_flowline, 26914), projected)
  stopifnot(all(lengths(parent) == 1L), length(unique(unlist(parent))) == 1L)
  parent <- unique(unlist(parent))
  # IDs encode the fixture HUC code for stable demonstrations only. They are not
  # enterprise identities, and do not make HUC codes mandatory identifiers.
  streams <- sf::st_sf(stream_id = paste0("00000000-0000-4000-8000-", huc$HUC12),
    study_area_id = study_area_id, stream_name = huc$Name, geometry = sf::st_geometry(huc))
  reaches <- data.frame(reach_id = reach_id, stream_id = streams$stream_id[parent],
    reach_name = "Cole Creek R1")
  study_area <- sf::st_sf(study_area_id = study_area_id, study_area_name = "NWO_Papillion",
    geometry = boundary)
  list(study_area = study_area, streams = streams, reaches = reaches,
    parent_index = parent,
    source_mapping = data.frame(source_layer = "Papillion_HUC12", HUC12 = huc$HUC12,
      source_name = huc$Name, provisional_stream_id = streams$stream_id),
    checks = list(valid_source_polygons = TRUE, valid_union = TRUE, union_components = 1L,
      union_area_m2 = union_area, source_minus_union_area_m2 = area_difference,
      numeric_area_tolerance_m2 = area_tolerance, all_source_polygons_covered = TRUE,
      stream_coordinates_unchanged = identical(sf::st_as_binary(sf::st_geometry(streams)),
        sf::st_as_binary(sf::st_geometry(huc))),
      derived_study_crs = "EPSG:26914", stream_source_crs = "EPSG:4269",
      inferred_reach_parent_huc12 = huc$HUC12[parent],
      inferred_reach_parent_name = huc$Name[parent]))
}
