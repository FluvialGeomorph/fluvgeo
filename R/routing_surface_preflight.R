.fg_routing_memory_estimate <- function(valid_cells,run_count,block_cells,
                                        fixed_headroom_mb=416,
                                        foreground_reserve_mb=960) {
  mib <- 1024^2
  parts <- c(
    float32_elevations=4*valid_cells,
    uint8_directions=valid_cells,
    float64_accumulation=8*valid_cells,
    uint32_indegree=4*valid_cells,
    uint32_work_queue=4*valid_cells,
    row_run_index=24*run_count,
    two_raster_blocks=16*block_cells,
    observed_worker_transient=128*mib,
    fixed_worker_headroom=fixed_headroom_mb*mib,
    foreground_session_reserve=foreground_reserve_mb*mib
  )
  list(bytes=parts,total_bytes=sum(parts),total_mib=sum(parts)/mib)
}

# Read a file-backed DEM by row blocks and discover its compact valid-cell runs.
# This is deliberately internal until the routing-surface contract is accepted.
.fg_routing_surface_preflight <- function(source,job_memory_mb=1024,
                                          safety_fraction=.75,
                                          block_memory_mb=16,
                                          fixed_headroom_mb=416,
                                          foreground_reserve_mb=960) {
  total_started <- proc.time()[["elapsed"]]
  scalar <- function(x) length(x)==1L && is.numeric(x) && is.finite(x)
  if(length(source)!=1L || is.na(source) || !nzchar(source) || !file.exists(source))
    stop("Supply one existing file-backed DEM.")
  if(!scalar(job_memory_mb) || job_memory_mb<=0 ||
     !scalar(safety_fraction) || safety_fraction<=0 || safety_fraction>=1 ||
     !scalar(block_memory_mb) || block_memory_mb<=0 ||
     !scalar(fixed_headroom_mb) || fixed_headroom_mb<0 ||
     !scalar(foreground_reserve_mb) || foreground_reserve_mb<0)
    stop("Supply positive memory limits and a safety fraction between zero and one.")

  source <- normalizePath(source,mustWork=TRUE)
  stage_started <- proc.time()[["elapsed"]]
  before <- .fg_file_sha256(source)
  hash_before_seconds <- proc.time()[["elapsed"]]-stage_started
  dem <- terra::rast(source)
  if(terra::nlyr(dem)!=1L || terra::is.lonlat(dem) || !length(terra::sources(dem)))
    stop("Use one file-backed projected DEM layer.")
  nr <- terra::nrow(dem); nc <- terra::ncol(dem)
  if(!is.finite(nr*nc) || nr*nc>2^53-1) stop("DEM dimensions exceed exact cell indexing.")

  rows_per_block <- max(1L,min(nr,floor(block_memory_mb*1024^2/(8*nc))))
  run_rows <- list(); valid_cells <- 0; run_count <- 0L
  minimum <- Inf; maximum <- -Inf; maximum_runs <- 0L
  scan_started <- proc.time()[["elapsed"]]
  started <- FALSE
  tryCatch({
    terra::readStart(dem); started <- TRUE
    for(row in seq.int(1L,nr,by=rows_per_block)) {
      nrows <- min(rows_per_block,nr-row+1L)
      values <- matrix(terra::readValues(dem,row=row,nrows=nrows),
        nrow=nrows,byrow=TRUE)
      if(any(!is.na(values) & !is.finite(values)))
        stop("DEM contains non-finite values that are not NoData.")
      for(i in seq_len(nrows)) {
        ok <- !is.na(values[i,])
        if(!any(ok)) next
        starts <- which(ok & !c(FALSE,ok[-length(ok)]))
        ends <- which(ok & !c(ok[-1L],FALSE))
        count <- sum(ok)
        first <- valid_cells+c(1,1+cumsum((ends-starts+1)[-length(starts)]))
        run_rows[[length(run_rows)+1L]] <- data.frame(
          row=rep(row+i-1L,length(starts)),col_start=starts,col_end=ends,
          compact_start=first
        )
        run_count <- run_count+length(starts)
        maximum_runs <- max(maximum_runs,length(starts))
        valid_cells <- valid_cells+count
        finite <- values[i,ok]
        minimum <- min(minimum,finite); maximum <- max(maximum,finite)
      }
    }
  },finally={
    if(started) terra::readStop(dem)
  })
  scan_seconds <- proc.time()[["elapsed"]]-scan_started
  if(!valid_cells) stop("DEM has no valid elevation cells.")
  runs <- do.call(rbind,run_rows)
  rownames(runs) <- NULL
  block_cells <- min(nr,rows_per_block)*nc
  estimate <- .fg_routing_memory_estimate(valid_cells,nrow(runs),block_cells,
    fixed_headroom_mb=fixed_headroom_mb,
    foreground_reserve_mb=foreground_reserve_mb)
  engine_bytes <- estimate$total_bytes-
    estimate$bytes[["fixed_worker_headroom"]]-
    estimate$bytes[["foreground_session_reserve"]]
  limit_mib <- job_memory_mb*safety_fraction
  stage_started <- proc.time()[["elapsed"]]
  after <- .fg_file_sha256(source)
  hash_after_seconds <- proc.time()[["elapsed"]]-stage_started
  if(!identical(before,after)) stop("DEM changed during routing preflight.")

  list(
    schema="ROUTING_SURFACE_PREFLIGHT_1",
    status=if(estimate$total_mib<=limit_mib) "PASS" else "BLOCKED",
    source=source,source_sha256=before,
    dimensions=c(rows=nr,columns=nc,cells=nr*nc),
    valid_cells=valid_cells,valid_fraction=valid_cells/(nr*nc),
    valid_runs=nrow(runs),maximum_runs_per_row=maximum_runs,
    elevation_range=c(minimum=minimum,maximum=maximum),runs=runs,
    block_plan=list(rows_per_block=rows_per_block,maximum_cells=block_cells,
      requested_block_mib=block_memory_mb),
    memory=list(job_mib=job_memory_mb,safety_fraction=safety_fraction,
      usable_mib=limit_mib,worker_headroom_mib=fixed_headroom_mb,
      foreground_reserve_mib=foreground_reserve_mb,
      estimated_engine_mib=engine_bytes/1024^2,
      estimated_peak_mib=estimate$total_mib,parts_bytes=as.list(estimate$bytes)),
    refusal=if(estimate$total_mib<=limit_mib) NULL else sprintf(
      "Estimated peak %.1f MiB exceeds the %.1f MiB safe routing allowance.",
      estimate$total_mib,limit_mib),
    processing_authorized=FALSE,
    method="terra row-block valid-domain discovery; no terrain modification",
    timings_seconds=list(hash_before=hash_before_seconds,scan=scan_seconds,
      hash_after=hash_after_seconds,total=proc.time()[["elapsed"]]-total_started),
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      terra=as.character(utils::packageVersion("terra")))
  )
}
