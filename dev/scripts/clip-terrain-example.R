# From the workspace root after source-loading fluvgeo. Supply a new output root.
# The AOI is deliberately synthetic; it is not the official Cole Creek Reach area.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, !file.exists(args[2]), dir.exists(dirname(args[2])))
source <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
stopifnot(dir.create(args[2]))
root <- normalizePath(args[2], winslash = "/", mustWork = TRUE)
r <- terra::rast(source)
e <- as.vector(terra::ext(r)); dx <- e[2]-e[1]; dy <- e[4]-e[3]
ring <- rbind(c(e[1]+.2*dx,e[3]+.2*dy),c(e[1]+.8*dx,e[3]+.2*dy),
  c(e[1]+.8*dx,e[3]+.65*dy),c(e[1]+.5*dx,e[3]+.8*dy),
  c(e[1]+.2*dx,e[3]+.65*dy),c(e[1]+.2*dx,e[3]+.2*dy))
aoi <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(ring)),crs=terra::crs(r)))
result <- fluvgeo::clip_terrain_to_aoi(source,aoi,file.path(root,"clip"),
  "DEVELOPMENT EXAMPLE: synthetic interior AOI exercises actual clipping of a retained Cole Creek terrain export; not an approved Study Area or Reach boundary.",
  touches=TRUE,report=TRUE)
print(result)
