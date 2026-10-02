#' Locate a terrain-routing outlet near the downstream Stream cap
#'
#' Uses the saved NHDPlusV2 reference chain only to identify the approximate
#' downstream cap. The selected outlet is the lowest valid DEM boundary cell
#' near the point where the next downstream NLDI flowline crosses that cap. If
#' NLDI has no continuation, the lowest cap cell near the terminal reference
#' endpoint is used. Reference hydrography does not determine the extracted path.
#'
#' @param dem Existing projected, single-band Hydro DEM path.
#' @param reference_lines CRS-defined NHDPlusV2 lines with `source_id`.
#' @param search_radius_m Radius around the reference endpoint used to inspect
#'   the Stream cap.
#' @param crossing_radius_m Maximum boundary-cell distance from the downstream
#'   reference crossing.
#' @param downstream_lines Optional downstream NLDI lines for deterministic or
#'   offline execution. When NULL, NLDI is queried from the terminal COMID.
#' @return One-row data frame containing the selected raster cell and evidence.
#' @export
locate_stream_outlet <- function(dem,reference_lines,search_radius_m=200,
                                 crossing_radius_m=30,downstream_lines=NULL) {
  if(length(dem)!=1L || is.na(dem) || !file.exists(dem))
    stop("Supply one existing Hydro DEM.")
  if(!inherits(reference_lines,"sf") || is.na(sf::st_crs(reference_lines)) ||
     !"source_id" %in% names(reference_lines) || !nrow(reference_lines))
    stop("Supply CRS-defined NHDPlusV2 reference lines with source_id.")
  if(anyNA(reference_lines$source_id) ||
     any(!grepl("^[0-9]+$",as.character(reference_lines$source_id))) ||
     !is.numeric(search_radius_m) || length(search_radius_m)!=1L ||
     !is.finite(search_radius_m) || search_radius_m<=0 ||
     !is.numeric(crossing_radius_m) || length(crossing_radius_m)!=1L ||
     !is.finite(crossing_radius_m) || crossing_radius_m<=0 ||
     crossing_radius_m>=search_radius_m)
    stop("Supply valid source IDs and positive crossing/search radii.")
  raster <- terra::rast(dem)
  if(terra::nlyr(raster)!=1L || terra::is.lonlat(raster))
    stop("Use one projected Hydro DEM layer.")
  lines <- sf::st_transform(reference_lines,terra::crs(raster))
  ids <- unique(as.character(lines$source_id))
  merged <- lapply(ids,function(id) {
    geometry <- sf::st_union(sf::st_geometry(lines[as.character(lines$source_id)==id,]))
    if(inherits(geometry,"sfc_LINESTRING")) geometry[[1]] else
      sf::st_line_merge(geometry)[[1]]
  })
  lines <- sf::st_sf(source_id=ids,geometry=sf::st_sfc(merged,
    crs=sf::st_crs(lines)))
  lines <- suppressWarnings(sf::st_cast(lines,"LINESTRING"))
  if(anyDuplicated(lines$source_id))
    stop("A reference source ID does not form one continuous line.")
  ordered <- order_drainage_flowlines(lines,direction="downstream",id_column="source_id")
  if(!nrow(ordered) || any(ordered$order_status!="ordered"))
    stop("Reference-line topology does not establish one downstream chain.")
  terminal <- ordered$source_row[which.max(ordered$navigation_order)]
  terminal_id <- as.character(lines$source_id[terminal])
  terminal_xy <- sf::st_coordinates(lines[terminal,])[,1:2,drop=FALSE]
  endpoint <- terminal_xy[nrow(terminal_xy),]

  if(is.null(downstream_lines)) downstream_lines <- tryCatch(
    drainage_navigation_service(terminal_id,"DM",1),error=function(e) NULL)
  next_line <- NULL;next_id <- NA_character_
  if(inherits(downstream_lines,"sf") && nrow(downstream_lines) &&
     !is.na(sf::st_crs(downstream_lines))) {
    id_field <- intersect(c("nhdplus_comid","comid","source_id"),names(downstream_lines))[1]
    if(!is.na(id_field)) {
      downstream_lines <- downstream_lines[
        as.character(downstream_lines[[id_field]])!=terminal_id,,drop=FALSE]
      if(nrow(downstream_lines)) {
        next_lines <- sf::st_transform(downstream_lines,terra::crs(raster))
        distances <- vapply(sf::st_geometry(next_lines),function(g) {
          xy <- sf::st_coordinates(g)[,1:2,drop=FALSE]
          min(sqrt(rowSums(sweep(xy,2,endpoint,"-")^2)))
        },numeric(1))
        next_row <- which(distances==min(distances))
        if(length(next_row)==1L) {
          next_line <- next_lines[next_row,,drop=FALSE]
          next_id <- as.character(downstream_lines[[id_field]][next_row])
        }
      }
    }
  }

  window <- terra::crop(raster,terra::ext(endpoint[1]+c(-search_radius_m,search_radius_m),
    endpoint[2]+c(-search_radius_m,search_radius_m)),snap="out")
  values <- terra::values(window,mat=FALSE)
  valid <- matrix(!is.na(values),nrow=terra::nrow(window),byrow=TRUE)
  pad <- matrix(FALSE,nrow(valid)+2L,ncol(valid)+2L)
  pad[2:(nrow(valid)+1L),2:(ncol(valid)+1L)] <- valid
  boundary <- valid
  for(dr in -1:1) for(dc in -1:1) if(dr!=0 || dc!=0)
    boundary <- boundary & pad[(2+dr):(nrow(valid)+1L+dr),(2+dc):(ncol(valid)+1L+dc)]
  boundary <- valid & !boundary
  cells <- which(as.vector(t(boundary)))
  if(!length(cells)) stop("No valid Stream-cap cells occur near the downstream reference endpoint.")
  xy <- terra::xyFromCell(window,cells)
  z <- values[cells]
  crossing_xy <- endpoint;method <-
    "Lowest Hydro DEM boundary cell near terminal NHDPlus endpoint (NLDI continuation unavailable)"
  if(!is.null(next_line)) {
    domain <- sf::st_as_sf(terra::as.polygons(terra::ifel(is.na(window),NA,1),
      aggregate=TRUE,values=FALSE,na.rm=TRUE))
    crossing <- suppressWarnings(sf::st_intersection(sf::st_geometry(next_line),
      sf::st_boundary(sf::st_union(sf::st_geometry(domain)))))
    if(any(!sf::st_geometry_type(crossing) %in% "POINT"))
      crossing <- sf::st_collection_extract(crossing,"POINT",warn=FALSE)
    if(length(crossing)==1L) {
      crossing_xy <- sf::st_coordinates(crossing)[1,1:2]
      method <- "Lowest Hydro DEM boundary cell near next-downstream NLDI cap crossing"
    } else next_id <- NA_character_
  }
  distance <- sqrt(rowSums(sweep(xy,2,crossing_xy,"-")^2))
  local <- which(distance<=if(is.na(next_id)) search_radius_m else crossing_radius_m)
  if(!length(local)) stop("No valid DEM boundary cell occurs near the downstream crossing.")
  candidates <- local[z[local]==min(z[local])]
  selected <- candidates[order(distance[candidates],cells[candidates])][1]
  global_cell <- terra::cellFromXY(raster,matrix(xy[selected,],nrow=1))
  data.frame(cell=global_cell,x=xy[selected,1],y=xy[selected,2],
    elevation=z[selected],terminal_source_id=terminal_id,next_source_id=next_id,
    crossing_x=crossing_xy[1],crossing_y=crossing_xy[2],
    crossing_distance_m=distance[selected],candidate_cells=length(local),
    method=method)
}

.fg_horizontal_metres <- function(raster) {
  unit <- tolower(sf::st_crs(terra::crs(raster))$units_gdal)
  if(unit %in% c("metre","meter","metres","meters","m")) return(1)
  if(unit %in% c("foot","feet","ft","international foot")) return(.3048)
  if(unit %in% c("us survey foot","us_survey_foot")) return(1200/3937)
  stop("The DEM horizontal unit cannot be converted to metres.")
}

.fg_stream_edges <- function(direction,accumulation,threshold) {
  x <- c(terra::rast(direction),terra::rast(accumulation))
  if(!isTRUE(terra::compareGeom(x[[1]],x[[2]],stopOnError=FALSE)))
    stop("Direction and accumulation grids differ.")
  plan <- terra::blocks(x,n=32); pieces <- vector("list",plan$n)
  terra::readStart(x); on.exit(terra::readStop(x),add=TRUE)
  for(i in seq_len(plan$n)) {
    value <- terra::readValues(x,row=plan$row[i],nrows=plan$nrows[i],mat=TRUE)
    keep <- !is.na(value[,1]) & value[,1]>0 & !is.na(value[,2]) & value[,2]>=threshold
    if(any(keep)) {
      cell <- (plan$row[i]-1)*terra::ncol(x)+which(keep)
      pieces[[i]] <- data.frame(cell=cell,terra::xyFromCell(x,cell),
        direction=as.integer(value[keep,1]),accumulation_cells=value[keep,2])
    }
  }
  stream <- do.call(rbind,pieces)
  if(is.null(stream) || !nrow(stream)) stop("No stream cells meet the contributing-area threshold.")
  dx <- c(`1`=1,`2`=1,`4`=0,`8`=-1,`16`=-1,`32`=-1,`64`=0,`128`=1)
  dy <- c(`1`=0,`2`=-1,`4`=-1,`8`=-1,`16`=0,`32`=1,`64`=1,`128`=1)
  res <- terra::res(x)
  stream$xend <- stream$x+unname(dx[as.character(stream$direction)])*res[1]
  stream$yend <- stream$y+unname(dy[as.character(stream$direction)])*res[2]
  stream[is.finite(stream$xend)&is.finite(stream$yend),,drop=FALSE]
}

.fg_consolidate_stream_edges <- function(edges,grid) {
  start_xy <- as.matrix(edges[c("x","y")]); end_xy <- as.matrix(edges[c("xend","yend")])
  key <- function(x) paste(format(x[,1],digits=16,trim=TRUE),
    format(x[,2],digits=16,trim=TRUE),sep="|")
  next_edge <- match(key(end_xy),key(start_xy))
  indegree <- tabulate(next_edge[!is.na(next_edge)],nbins=nrow(edges))
  starts <- which(indegree!=1L); used <- rep(FALSE,nrow(edges))
  records <- geometries <- list(); reach <- 0L
  for(seed in starts) {
    if(used[seed]) next
    current <- seed; ids <- integer(); xy <- matrix(start_xy[current,],nrow=1)
    repeat {
      if(used[current]) stop("Thresholded stream graph contains a repeated edge.")
      used[current] <- TRUE; ids <- c(ids,current); xy <- rbind(xy,end_xy[current,])
      next_id <- next_edge[current]
      if(is.na(next_id) || indegree[next_id]!=1L) break
      current <- next_id
    }
    reach <- reach+1L; geometries[[reach]] <- sf::st_linestring(xy)
    records[[reach]] <- data.frame(stream_line_id=sprintf("SN%05d",reach),
      upstream_cell=edges$cell[ids[1]],downstream_cell=terra::cellFromXY(grid,
        matrix(end_xy[ids[length(ids)],],nrow=1)),
      segment_count=length(ids),upstream_accumulation_cells=edges$accumulation_cells[ids[1]],
      downstream_accumulation_cells=edges$accumulation_cells[ids[length(ids)]])
  }
  if(any(!used)) stop("Thresholded stream graph contains a cycle or unreachable edge.")
  network <- sf::st_sf(do.call(rbind,records),geometry=sf::st_sfc(geometries,
    crs=sf::st_crs(terra::crs(grid))))
  network$length_m <- as.numeric(units::set_units(sf::st_length(network),"m"))
  network
}

#' Apply a contributing-area threshold to existing routing outputs
#'
#' Reuses resolved D8 direction and accumulation rasters. It does not condition
#' terrain, recalculate flow direction or recalculate accumulation.
#'
#' @param direction Existing resolved D8 direction GeoTIFF.
#' @param accumulation Existing upstream-cell accumulation GeoTIFF.
#' @param output_file New GeoPackage path for the thresholded network.
#' @param threshold_ha Positive contributing-area threshold in hectares.
#' @return Threshold, cell-area and vector summary evidence.
#' @export
threshold_synthetic_stream_network <- function(direction,accumulation,output_file,
                                                threshold_ha=1) {
  if(length(direction)!=1L || !file.exists(direction) ||
     length(accumulation)!=1L || !file.exists(accumulation))
    stop("Supply existing direction and accumulation rasters.")
  if(!is.numeric(threshold_ha) || length(threshold_ha)!=1L ||
     !is.finite(threshold_ha) || threshold_ha<=0)
    stop("The contributing-area threshold must be positive hectares.")
  if(length(output_file)!=1L || !nzchar(output_file) || file.exists(output_file))
    stop("Supply a new output GeoPackage path.")
  grid <- terra::rast(direction); metres <- .fg_horizontal_metres(grid)
  cell_area_m2 <- prod(terra::res(grid)*metres)
  threshold_cells <- ceiling(threshold_ha*10000/cell_area_m2)
  edges <- .fg_stream_edges(direction,accumulation,threshold_cells)
  network <- .fg_consolidate_stream_edges(edges,grid)
  network$threshold_ha <- threshold_ha
  network$threshold_cells <- threshold_cells
  sf::st_write(network,output_file,"stream_network",quiet=TRUE)
  list(threshold_ha=threshold_ha,threshold_cells=threshold_cells,
    cell_area_m2=cell_area_m2,stream_lines=nrow(network),
    stream_length_m=sum(network$length_m),
    stream_network_sha256=.fg_file_sha256(output_file),
    completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
}

#' Extract a terrain-derived candidate stream network
#'
#' Runs compact Priority-Flood conditioning, native D8 direction and flat
#' resolution, cell-count accumulation, hectare thresholding and lossless
#' consolidation between heads, junctions and the outlet. The Hydro DEM remains
#' unchanged. Outputs are candidates for analyst review, not accepted FGDB data.
#'
#' @param dem Existing projected Hydro DEM path.
#' @param outlet_cell One reviewed outlet cell from [locate_stream_outlet()].
#' @param output_directory Existing empty job-owned directory.
#' @param threshold_ha Positive contributing-area threshold in hectares.
#' @param memory_budget_mb Total co-resident application/worker memory budget.
#' @param safety_fraction Fraction of the budget available to the job.
#' @return Reproducibility manifest with output paths and summary evidence.
#' @export
extract_synthetic_stream_network <- function(dem,outlet_cell,output_directory,
  threshold_ha=1,memory_budget_mb=3072,safety_fraction=.75) {
  if(length(output_directory)!=1L || !dir.exists(output_directory) ||
     length(list.files(output_directory,all.files=TRUE,no..=TRUE)))
    stop("Supply one existing empty output directory.")
  if(!is.numeric(threshold_ha) || length(threshold_ha)!=1L ||
     !is.finite(threshold_ha) || threshold_ha<=0)
    stop("The contributing-area threshold must be positive hectares.")
  dem <- normalizePath(dem,mustWork=TRUE); output_directory <- normalizePath(output_directory)
  routing <- file.path(output_directory,"routing.tif")
  fill <- file.path(output_directory,"fill-depth.tif")
  direction <- file.path(output_directory,"flow-direction.tif")
  accumulation <- file.path(output_directory,"flow-accumulation.tif")
  route <- .fg_priority_flood_route(dem,outlet_cell,routing,fill,
    job_memory_mb=memory_budget_mb,safety_fraction=safety_fraction,
    direction_filename=direction,accumulation_filename=accumulation)
  network_path <- file.path(output_directory,"stream-network.gpkg")
  threshold <- threshold_synthetic_stream_network(direction,accumulation,
    network_path,threshold_ha)
  result <- list(schema="SYNTHETIC_STREAM_NETWORK_1",source=dem,
    source_sha256=route$source_sha256,outlet_cell=as.numeric(outlet_cell),
    threshold_ha=threshold$threshold_ha,threshold_cells=threshold$threshold_cells,
    cell_area_m2=threshold$cell_area_m2,stream_lines=threshold$stream_lines,
    stream_length_m=threshold$stream_length_m,changed_cells=route$changed_cells,
    maximum_fill=route$maximum_fill,files=list(routing="routing.tif",
      fill_depth="fill-depth.tif",direction="flow-direction.tif",
      accumulation="flow-accumulation.tif",
      stream_network="stream-network.gpkg"),
    hashes=list(routing=route$routing_sha256,fill_depth=route$fill_depth_sha256,
      direction=route$direction_sha256,accumulation=route$accumulation_sha256,
      stream_network=threshold$stream_network_sha256),route=route,
    method="Priority-Flood; steepest-downslope D8; Barnes flat resolution; upstream-cell accumulation; hectare threshold; maximal topological lines",
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      terra=as.character(utils::packageVersion("terra")),R=R.version.string),
    completed=threshold$completed)
  saveRDS(result,file.path(output_directory,"result.rds"))
  json <- result; json$route <- NULL
  jsonlite::write_json(json,file.path(output_directory,"provenance.json"),
    auto_unbox=TRUE,pretty=TRUE,null="null")
  result
}
