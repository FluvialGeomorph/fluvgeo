# Profile the expensive terrain-routing stages on one real Hydro DEM.
# Usage: Rscript --vanilla dev/scripts/profile-priority-flood-stages.R SOURCE OUTLET_CELL OUTPUT_DIR

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop("Supply a Hydro DEM, one reviewed outlet cell and a new output directory.")
source <- normalizePath(args[1],mustWork=TRUE)
outlet <- as.numeric(args[2])
if(length(outlet)!=1L || !is.finite(outlet) || outlet<1 || outlet!=floor(outlet))
  stop("The outlet must be one positive whole-number cell index.")
output_dir <- normalizePath(args[3],winslash="/",mustWork=FALSE)
if(file.exists(output_dir)) stop("The profiling output directory already exists.")
dir.create(output_dir,recursive=TRUE)

stage_rows <- list()
measure <- function(stage,expression) {
  gc()
  started <- proc.time()
  value <- force(expression)
  elapsed <- proc.time()-started
  stage_rows[[length(stage_rows)+1L]] <<- data.frame(
    stage=stage,user_seconds=unname(elapsed[["user.self"]]),
    system_seconds=unname(elapsed[["sys.self"]]),
    elapsed_seconds=unname(elapsed[["elapsed"]]),stringsAsFactors=FALSE)
  utils::write.csv(do.call(rbind,stage_rows),
    file.path(output_dir,"stage-times.partial.csv"),row.names=FALSE)
  value
}

routing_path <- file.path(output_dir,"routing.tif")
fill_path <- file.path(output_dir,"fill-depth.tif")
direction_path <- file.path(output_dir,"flow-direction-ltd.tif")
resolved_path <- file.path(output_dir,"flow-direction-flat-resolved.tif")
accumulation_path <- file.path(output_dir,"flow-accumulation.tif")

route <- measure("priority_flood_route",
  fluvgeo:::.fg_priority_flood_route(source,outlet,routing_path,fill_path,
    job_memory_mb=1024,foreground_reserve_mb=0))
routing <- terra::rast(routing_path)

direction_warnings <- character()
direction <- withCallingHandlers(
  measure("terra_flow_direction_ltd",
    terra::flowDir(routing,lambda=.5,deviation_type="ltd",
      filename=direction_path,overwrite=FALSE,
      wopt=list(gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER")))),
  warning=function(w) {
    direction_warnings <<- c(direction_warnings,conditionMessage(w))
    invokeRestart("muffleWarning")
  })

flat <- measure("flat_resolution",
  fluvgeo:::.fg_resolve_routing_flats(routing_path,direction_path,outlet,resolved_path,
    job_memory_mb=1024,foreground_reserve_mb=0))
resolved <- terra::rast(resolved_path)
accumulation <- measure("terra_flow_accumulation",
  terra::flowAccumulation(resolved,filename=accumulation_path,overwrite=FALSE,
    wopt=list(datatype="FLT8S",gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))))

outlet_xy <- terra::xyFromCell(accumulation,outlet)
outlet_accumulation <- terra::extract(accumulation,outlet_xy)[1,1]
if(flat$resolution$unresolved_cells!=0 || outlet_accumulation!=route$preflight$valid_cells)
  stop("Profiled routing did not send every valid cell to the reviewed outlet.")

stages <- do.call(rbind,stage_rows)
stages$share_of_profiled_elapsed <- stages$elapsed_seconds/sum(stages$elapsed_seconds)
utils::write.csv(stages,file.path(output_dir,"stage-times.csv"),row.names=FALSE)
result <- list(
  schema="PRIORITY_FLOOD_PERFORMANCE_PROFILE_1",
  source=source,source_sha256=route$source_sha256,outlet_cell=outlet,
  dimensions=route$preflight$dimensions,valid_cells=route$preflight$valid_cells,
  valid_fraction=route$preflight$valid_fraction,
  estimated_memory=route$preflight$memory,
  stage_times=stages,route_timings=route$timings_seconds,
  flat_timings=flat$timings_seconds,direction_warnings=unique(direction_warnings),
  outlet_accumulation=outlet_accumulation,
  software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
    terra=as.character(utils::packageVersion("terra")),R=R.version.string),
  completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
saveRDS(result,file.path(output_dir,"profile.rds"))
writeLines(capture.output(str(result,max.level=2)),file.path(output_dir,"profile.txt"))
print(stages,row.names=FALSE)
