.fg_priority_create <- function(preflight) {
  .Call("fg_priority_create",
    as.integer(preflight$dimensions[["rows"]]),
    as.integer(preflight$dimensions[["columns"]]),
    as.numeric(preflight$valid_cells),
    as.integer(preflight$runs$row),
    as.integer(preflight$runs$col_start),
    as.integer(preflight$runs$col_end),
    as.numeric(preflight$runs$compact_start),PACKAGE="fluvgeo")
}

.fg_priority_load <- function(state,row,nrows,values) {
  invisible(.Call("fg_priority_load",state,as.integer(row),as.integer(nrows),
    as.numeric(values),PACKAGE="fluvgeo"))
}

.fg_priority_fill <- function(state,outlet_cells) {
  .Call("fg_priority_fill",state,as.numeric(outlet_cells),PACKAGE="fluvgeo")
}

.fg_priority_values <- function(state,row,nrows) {
  .Call("fg_priority_values",state,as.integer(row),as.integer(nrows),PACKAGE="fluvgeo")
}

.fg_priority_load_directions <- function(state,row,nrows,values) {
  invisible(.Call("fg_priority_load_directions",state,as.integer(row),as.integer(nrows),
    as.numeric(values),PACKAGE="fluvgeo"))
}

.fg_priority_assign_d8 <- function(state,resolution,outlet_cells) {
  .Call("fg_priority_assign_d8",state,as.numeric(resolution[[1]]),
    as.numeric(resolution[[2]]),as.numeric(outlet_cells),PACKAGE="fluvgeo")
}

.fg_priority_resolve_flats <- function(state,outlet_cells) {
  .Call("fg_priority_resolve_flats",state,as.numeric(outlet_cells),PACKAGE="fluvgeo")
}

.fg_priority_direction_values <- function(state,row,nrows) {
  .Call("fg_priority_direction_values",state,as.integer(row),as.integer(nrows),
    PACKAGE="fluvgeo")
}

.fg_priority_accumulate <- function(state) {
  .Call("fg_priority_accumulate",state,PACKAGE="fluvgeo")
}

.fg_priority_accumulation_values <- function(state,row,nrows) {
  .Call("fg_priority_accumulation_values",state,as.integer(row),as.integer(nrows),
    PACKAGE="fluvgeo")
}

# Internal integration boundary. Public API follows real-terrain review.
.fg_priority_flood_route <- function(source,outlet_cells,routing_filename,
                                     fill_filename,job_memory_mb=1024,
                                     safety_fraction=.75,block_memory_mb=16,
                                     fixed_headroom_mb=416,foreground_reserve_mb=960,
                                     direction_filename=NULL,
                                     accumulation_filename=NULL) {
  total_started <- proc.time()[["elapsed"]]
  include_direction <- !is.null(direction_filename)
  include_accumulation <- !is.null(accumulation_filename)
  if(include_accumulation && !include_direction)
    stop("Native accumulation requires a native direction output.")
  outputs <- c(routing_filename,fill_filename,
    if(include_direction) direction_filename else character(),
    if(include_accumulation) accumulation_filename else character())
  if(anyNA(outputs) || any(!nzchar(outputs)) || anyDuplicated(normalizePath(outputs,
    winslash="/",mustWork=FALSE)) || any(file.exists(outputs)))
    stop("Supply different new routing output paths.")
  parents <- normalizePath(dirname(outputs),winslash="/",mustWork=TRUE)
  if(length(unique(parents))!=1L) stop("Routing outputs must share one existing directory.")

  stage_started <- proc.time()[["elapsed"]]
  preflight <- .fg_routing_surface_preflight(source,job_memory_mb,safety_fraction,
    block_memory_mb,fixed_headroom_mb,foreground_reserve_mb)
  preflight_seconds <- proc.time()[["elapsed"]]-stage_started
  if(!identical(preflight$status,"PASS")) stop(preflight$refusal)
  if(!is.numeric(outlet_cells) || !length(outlet_cells) || anyNA(outlet_cells) ||
     any(!is.finite(outlet_cells)) || any(outlet_cells!=floor(outlet_cells)) ||
     anyDuplicated(outlet_cells)) stop("Supply unique integer outlet cell numbers.")

  stage_started <- proc.time()[["elapsed"]]
  state <- .fg_priority_create(preflight)
  state_create_seconds <- proc.time()[["elapsed"]]-stage_started
  dem <- terra::rast(source)
  rows_per_block <- preflight$block_plan$rows_per_block
  reading <- FALSE
  stage_started <- proc.time()[["elapsed"]]
  tryCatch({
    terra::readStart(dem); reading <- TRUE
    for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
      nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
      .fg_priority_load(state,row,nrows,terra::readValues(dem,row=row,nrows=nrows))
    }
  },finally={
    if(reading) terra::readStop(dem)
  })
  input_load_seconds <- proc.time()[["elapsed"]]-stage_started
  stage_started <- proc.time()[["elapsed"]]
  fill_evidence <- .fg_priority_fill(state,outlet_cells)
  native_fill_seconds <- proc.time()[["elapsed"]]-stage_started
  if(fill_evidence$visited_cells!=preflight$valid_cells)
    stop(sprintf("Outlet reaches %.0f of %.0f valid cells; disconnected terrain remains.",
      fill_evidence$visited_cells,preflight$valid_cells))

  direction_evidence <- flat_resolution <- NULL
  accumulation_evidence <- NULL
  native_direction_seconds <- native_flat_seconds <- native_accumulation_seconds <- 0
  if(include_direction) {
    stage_started <- proc.time()[["elapsed"]]
    direction_evidence <- .fg_priority_assign_d8(state,terra::res(dem),outlet_cells)
    native_direction_seconds <- proc.time()[["elapsed"]]-stage_started
    stage_started <- proc.time()[["elapsed"]]
    flat_resolution <- .fg_priority_resolve_flats(state,outlet_cells)
    native_flat_seconds <- proc.time()[["elapsed"]]-stage_started
    if(flat_resolution$unresolved_cells!=0)
      stop(sprintf("Native D8 routing left %.0f cells unresolved.",
        flat_resolution$unresolved_cells))
    if(include_accumulation) {
      stage_started <- proc.time()[["elapsed"]]
      accumulation_evidence <- .fg_priority_accumulate(state)
      native_accumulation_seconds <- proc.time()[["elapsed"]]-stage_started
      if(accumulation_evidence$processed_cells!=preflight$valid_cells ||
         accumulation_evidence$terminal_cells!=length(outlet_cells))
        stop("Native flow accumulation did not produce the expected outlet topology.")
    }
  }

  scratch <- tempfile("priority-flood-",parents[1]);dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  staged <- file.path(scratch,c("routing.tif","fill-depth.tif",
    if(include_direction) "flow-direction.tif" else character(),
    if(include_accumulation) "flow-accumulation.tif" else character()))
  routing <- terra::rast(dem)
  terra::units(routing) <- terra::units(dem)
  routing_open <- FALSE
  stage_started <- proc.time()[["elapsed"]]
  tryCatch({
    terra::writeStart(routing,staged[1],overwrite=FALSE,
      datatype="FLT4S",gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
    routing_open <- TRUE
    for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
      nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
      terra::writeValues(routing,.fg_priority_values(state,row,nrows),row,nrows)
    }
  },finally={
    if(routing_open) terra::writeStop(routing)
  })
  routing_write_seconds <- proc.time()[["elapsed"]]-stage_started

  depth <- terra::rast(dem)
  terra::units(depth) <- terra::units(dem)
  depth_open <- reading <- FALSE
  changed <- 0; maximum <- 0; fill_integral <- 0
  stage_started <- proc.time()[["elapsed"]]
  tryCatch({
    terra::writeStart(depth,staged[2],overwrite=FALSE,
      datatype="FLT4S",gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
    depth_open <- TRUE
    terra::readStart(dem);reading <- TRUE
    for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
      nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
      original <- terra::readValues(dem,row=row,nrows=nrows)
      filled <- .fg_priority_values(state,row,nrows)
      delta <- filled-original
      positive <- is.finite(delta) & delta>0
      changed <- changed+sum(positive)
      if(any(positive)) {
        maximum <- max(maximum,delta[positive])
        fill_integral <- fill_integral+sum(delta[positive])*prod(terra::res(dem))
      }
      terra::writeValues(depth,delta,row,nrows)
    }
  },finally={
    if(reading) terra::readStop(dem)
    if(depth_open) terra::writeStop(depth)
  })
  fill_depth_write_seconds <- proc.time()[["elapsed"]]-stage_started

  direction_write_seconds <- 0
  if(include_direction) {
    direction <- terra::rast(dem)
    terra::units(direction) <- ""
    direction_open <- FALSE
    stage_started <- proc.time()[["elapsed"]]
    tryCatch({
      terra::writeStart(direction,staged[3],overwrite=FALSE,datatype="INT2U",NAflag=65535,
        gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
      direction_open <- TRUE
      for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
        nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
        terra::writeValues(direction,.fg_priority_direction_values(state,row,nrows),row,nrows)
      }
    },finally={if(direction_open) terra::writeStop(direction)})
    direction_write_seconds <- proc.time()[["elapsed"]]-stage_started
  }
  accumulation_write_seconds <- 0
  if(include_accumulation) {
    accumulation <- terra::rast(dem)
    terra::units(accumulation) <- "cells"
    accumulation_open <- FALSE
    stage_started <- proc.time()[["elapsed"]]
    tryCatch({
      terra::writeStart(accumulation,staged[4],overwrite=FALSE,datatype="FLT8S",
        gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
      accumulation_open <- TRUE
      for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
        nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
        terra::writeValues(accumulation,
          .fg_priority_accumulation_values(state,row,nrows),row,nrows)
      }
    },finally={if(accumulation_open) terra::writeStop(accumulation)})
    accumulation_write_seconds <- proc.time()[["elapsed"]]-stage_started
  }
  finalize_started <- proc.time()[["elapsed"]]
  if(!identical(preflight$source_sha256,.fg_file_sha256(source)))
    stop("DEM changed while writing the routing result.")
  routed <- terra::rast(staged[1]);filled_depth <- terra::rast(staged[2])
  if(!isTRUE(terra::compareGeom(dem,routed,stopOnError=FALSE)) ||
     !isTRUE(terra::compareGeom(dem,filled_depth,stopOnError=FALSE)))
    stop("Routing output grid changed unexpectedly.")
  if(include_direction &&
     !isTRUE(terra::compareGeom(dem,terra::rast(staged[3]),stopOnError=FALSE)))
    stop("Flow directions changed the routing grid unexpectedly.")
  if(include_accumulation &&
     !isTRUE(terra::compareGeom(dem,terra::rast(staged[4]),stopOnError=FALSE)))
    stop("Flow accumulation changed the routing grid unexpectedly.")
  if(!identical(terra::units(dem),terra::units(routed)) ||
     !identical(terra::units(dem),terra::units(filled_depth)))
    stop("Routing output elevation units changed unexpectedly.")
  if(!file.rename(staged[1],outputs[1])) stop("Could not publish the routing surface.")
  if(!file.rename(staged[2],outputs[2])) {
    unlink(outputs[1])
    stop("Could not publish the fill-depth raster.")
  }
  if(include_direction && !file.rename(staged[3],outputs[3])) {
    unlink(outputs[1:2])
    stop("Could not publish the flow-direction raster.")
  }
  if(include_accumulation && !file.rename(staged[4],outputs[4])) {
    unlink(outputs[1:3])
    stop("Could not publish the flow-accumulation raster.")
  }
  summary <- preflight
  summary$runs <- NULL
  result <- list(schema=if(include_accumulation) "PRIORITY_FLOOD_ROUTE_3" else
      if(include_direction) "PRIORITY_FLOOD_ROUTE_2" else "PRIORITY_FLOOD_ROUTE_1",
    source=normalizePath(source),
    source_sha256=preflight$source_sha256,outlet_cells=outlet_cells,
    routing=normalizePath(outputs[1]),routing_sha256=.fg_file_sha256(outputs[1]),
    fill_depth=normalizePath(outputs[2]),fill_depth_sha256=.fg_file_sha256(outputs[2]),
    changed_cells=changed,maximum_fill=maximum,fill_integral=fill_integral,
    fill_integral_units=paste0(terra::units(dem)," * map-unit^2"),
    native=fill_evidence,preflight=summary,
    timings_seconds=list(preflight=preflight_seconds,state_create=state_create_seconds,
      input_load=input_load_seconds,native_fill=native_fill_seconds,
      routing_write=routing_write_seconds,fill_depth_write=fill_depth_write_seconds,
      native_direction=native_direction_seconds,native_flat_resolution=native_flat_seconds,
      native_accumulation=native_accumulation_seconds,
      direction_write=direction_write_seconds,accumulation_write=accumulation_write_seconds,
      finalize=proc.time()[["elapsed"]]-finalize_started,
      total=proc.time()[["elapsed"]]-total_started),
    method=if(include_direction)
      paste("8-neighbor improved Priority-Flood; strict steepest-downslope D8;",
        "Barnes-Lehman-Mulla flat mask; explicit outlet; other boundaries closed") else
      "8-neighbor improved Priority-Flood; explicit outlet; other boundaries closed",
    completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  if(include_direction) {
    result$direction <- normalizePath(outputs[3])
    result$direction_sha256 <- .fg_file_sha256(outputs[3])
    result$direction_evidence <- direction_evidence
    result$flat_resolution <- flat_resolution
  }
  if(include_accumulation) {
    result$accumulation <- normalizePath(outputs[4])
    result$accumulation_sha256 <- .fg_file_sha256(outputs[4])
    result$accumulation_evidence <- accumulation_evidence
  }
  result
}

# Resolve fill-created flats using Barnes, Lehman and Mulla's integer mask.
# The routing elevations are read but never modified.
.fg_resolve_routing_flats <- function(routing,initial_direction,outlet_cells,
                                      direction_filename,job_memory_mb=1024,
                                      safety_fraction=.75,block_memory_mb=16,
                                      fixed_headroom_mb=416,
                                      foreground_reserve_mb=960) {
  total_started <- proc.time()[["elapsed"]]
  inputs <- c(routing,initial_direction)
  if(anyNA(inputs) || any(!file.exists(inputs))) stop("Supply existing routing and direction rasters.")
  if(length(direction_filename)!=1L || is.na(direction_filename) || !nzchar(direction_filename) ||
     file.exists(direction_filename)) stop("Supply one new flow-direction output path.")
  normalizePath(dirname(direction_filename),mustWork=TRUE)
  routing <- normalizePath(routing,mustWork=TRUE)
  initial_direction <- normalizePath(initial_direction,mustWork=TRUE)
  stage_started <- proc.time()[["elapsed"]]
  before <- c(routing=.fg_file_sha256(routing),direction=.fg_file_sha256(initial_direction))
  input_hash_seconds <- proc.time()[["elapsed"]]-stage_started
  stage_started <- proc.time()[["elapsed"]]
  preflight <- .fg_routing_surface_preflight(routing,job_memory_mb,safety_fraction,
    block_memory_mb,fixed_headroom_mb,foreground_reserve_mb)
  preflight_seconds <- proc.time()[["elapsed"]]-stage_started
  if(!identical(preflight$status,"PASS")) stop(preflight$refusal)
  dem <- terra::rast(routing)
  direction <- terra::rast(initial_direction)
  if(terra::nlyr(direction)!=1L ||
     !isTRUE(terra::compareGeom(dem,direction,stopOnError=FALSE)))
    stop("Initial directions must share the routing grid.")
  if(!is.numeric(outlet_cells) || !length(outlet_cells) || anyNA(outlet_cells) ||
     any(!is.finite(outlet_cells)) || any(outlet_cells!=floor(outlet_cells)) ||
     anyDuplicated(outlet_cells)) stop("Supply unique integer outlet cell numbers.")

  stage_started <- proc.time()[["elapsed"]]
  state <- .fg_priority_create(preflight)
  state_create_seconds <- proc.time()[["elapsed"]]-stage_started
  rows_per_block <- preflight$block_plan$rows_per_block
  dem_open <- direction_open <- FALSE
  stage_started <- proc.time()[["elapsed"]]
  tryCatch({
    terra::readStart(dem);dem_open <- TRUE
    terra::readStart(direction);direction_open <- TRUE
    for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
      nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
      .fg_priority_load(state,row,nrows,terra::readValues(dem,row=row,nrows=nrows))
      .fg_priority_load_directions(state,row,nrows,
        terra::readValues(direction,row=row,nrows=nrows))
    }
  },finally={
    if(dem_open) terra::readStop(dem)
    if(direction_open) terra::readStop(direction)
  })
  input_load_seconds <- proc.time()[["elapsed"]]-stage_started
  started <- proc.time()[["elapsed"]]
  resolution <- .fg_priority_resolve_flats(state,outlet_cells)
  resolution$elapsed_seconds <- proc.time()[["elapsed"]]-started

  scratch <- tempfile("flat-direction-",dirname(direction_filename));dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  staged <- file.path(scratch,"flow-direction.tif")
  output <- terra::rast(dem)
  terra::units(output) <- ""
  output_open <- FALSE
  stage_started <- proc.time()[["elapsed"]]
  tryCatch({
    terra::writeStart(output,staged,overwrite=FALSE,datatype="INT2U",NAflag=65535,
      gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
    output_open <- TRUE
    for(row in seq.int(1L,terra::nrow(dem),by=rows_per_block)) {
      nrows <- min(rows_per_block,terra::nrow(dem)-row+1L)
      terra::writeValues(output,.fg_priority_direction_values(state,row,nrows),row,nrows)
    }
  },finally={if(output_open) terra::writeStop(output)})
  output_write_seconds <- proc.time()[["elapsed"]]-stage_started
  finalize_started <- proc.time()[["elapsed"]]
  after <- c(routing=.fg_file_sha256(routing),direction=.fg_file_sha256(initial_direction))
  if(!identical(before,after)) stop("Routing inputs changed during flat resolution.")
  if(!isTRUE(terra::compareGeom(dem,terra::rast(staged),stopOnError=FALSE)))
    stop("Resolved directions changed the routing grid.")
  if(!file.rename(staged,direction_filename)) stop("Could not publish resolved directions.")
  summary <- preflight;summary$runs <- NULL
  list(schema="FLAT_RESOLUTION_1",routing=routing,routing_sha256=before[["routing"]],
    initial_direction=initial_direction,initial_direction_sha256=before[["direction"]],
    outlet_cells=outlet_cells,direction=normalizePath(direction_filename),
    direction_sha256=.fg_file_sha256(direction_filename),resolution=resolution,
    preflight=summary,
    timings_seconds=list(input_hash=input_hash_seconds,preflight=preflight_seconds,
      state_create=state_create_seconds,input_load=input_load_seconds,
      native_resolution=resolution$elapsed_seconds,output_write=output_write_seconds,
      finalize=proc.time()[["elapsed"]]-finalize_started,
      total=proc.time()[["elapsed"]]-total_started),
    method="Barnes-Lehman-Mulla integer flat mask; D8; elevations unchanged",
    completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
}
