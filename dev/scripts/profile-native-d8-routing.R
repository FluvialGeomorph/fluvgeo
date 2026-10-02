# Profile the integrated native Priority-Flood, D8 and flat-resolution path.
# Usage: Rscript --vanilla dev/scripts/profile-native-d8-routing.R SOURCE OUTLET_CELL OUTPUT_DIR

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop("Supply a Hydro DEM, one reviewed outlet cell and a new output directory.")
source <- normalizePath(args[1],mustWork=TRUE)
outlet <- as.numeric(args[2])
if(length(outlet)!=1L || !is.finite(outlet) || outlet<1 || outlet!=floor(outlet))
  stop("The outlet must be one positive whole-number cell index.")
output_dir <- normalizePath(args[3],winslash="/",mustWork=FALSE)
if(file.exists(output_dir)) stop("The profiling output directory already exists.")
dir.create(output_dir,recursive=TRUE)

routing_path <- file.path(output_dir,"routing.tif")
fill_path <- file.path(output_dir,"fill-depth.tif")
direction_path <- file.path(output_dir,"flow-direction-flat-resolved.tif")
accumulation_path <- file.path(output_dir,"flow-accumulation-flat-resolved.tif")

gc()
started <- proc.time()
route <- fluvgeo:::.fg_priority_flood_route(source,outlet,routing_path,fill_path,
  job_memory_mb=1024,foreground_reserve_mb=0,direction_filename=direction_path,
  accumulation_filename=accumulation_path)
route_elapsed <- proc.time()-started
accumulation <- terra::rast(accumulation_path)
outlet_xy <- terra::xyFromCell(accumulation,outlet)
outlet_accumulation <- terra::extract(accumulation,outlet_xy)[1,1]
if(route$flat_resolution$unresolved_cells!=0 ||
   outlet_accumulation!=route$preflight$valid_cells)
  stop("Integrated routing did not send every valid cell to the reviewed outlet.")

stage_times <- data.frame(
  stage="integrated_priority_flood_native_d8_accumulation",
  user_seconds=unname(route_elapsed[["user.self"]]),
  system_seconds=unname(route_elapsed[["sys.self"]]),
  elapsed_seconds=unname(route_elapsed[["elapsed"]])
)
stage_times$share_of_profiled_elapsed <- 1
utils::write.csv(stage_times,file.path(output_dir,"stage-times.csv"),row.names=FALSE)

flat_result <- list(
  schema="INTEGRATED_NATIVE_D8_FLAT_RESOLUTION_1",
  outlet_cells=route$outlet_cells,preflight=route$preflight,
  resolution=c(route$flat_resolution,
    elapsed_seconds=route$timings_seconds$native_flat_resolution)
)
accumulation_range <- c(1,route$accumulation_evidence$maximum_accumulation)
flat_evidence <- list(
  accumulation_range=accumulation_range,
  outlet_accumulation=outlet_accumulation,
  accumulation_seconds=route$timings_seconds$native_accumulation+
    route$timings_seconds$accumulation_write
)
saveRDS(route,file.path(output_dir,"result.rds"))
saveRDS(flat_result,file.path(output_dir,"flat-resolution-result.rds"))
saveRDS(flat_evidence,file.path(output_dir,"flat-routing-evidence.rds"))
saveRDS(list(schema="NATIVE_D8_PERFORMANCE_PROFILE_1",route=route,
  stage_times=stage_times,outlet_accumulation=outlet_accumulation,
  software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
    terra=as.character(utils::packageVersion("terra")),R=R.version.string),
  completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")),
  file.path(output_dir,"profile.rds"))
writeLines(capture.output(list(route_timings=route$timings_seconds,
  stage_times=stage_times,direction_evidence=route$direction_evidence,
  flat_resolution=route$flat_resolution,outlet_accumulation=outlet_accumulation)),
  file.path(output_dir,"profile.txt"))
print(stage_times,row.names=FALSE)
print(route$timings_seconds)
