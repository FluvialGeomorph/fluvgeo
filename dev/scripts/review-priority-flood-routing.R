# Create review figures from one completed real-terrain routing diagnostic.
# Usage: Rscript --vanilla dev/scripts/review-priority-flood-routing.R RESULT_DIR THRESHOLD

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply the completed result directory and accumulation threshold (cells).")
result_dir <- normalizePath(args[1],mustWork=TRUE)
threshold <- as.numeric(args[2])
if(!is.finite(threshold) || threshold<1 || threshold!=floor(threshold))
  stop("The accumulation threshold must be a positive whole number of cells.")

paths <- file.path(result_dir,c("result.rds","routing-evidence.rds","routing.tif",
  "fill-depth.tif","flow-direction.tif","flow-accumulation.tif","pits.tif"))
if(any(!file.exists(paths))) stop("The result directory is incomplete.")
names(paths) <- c("result","evidence","routing","fill","direction","accumulation","pits")
result <- readRDS(paths["result"])
evidence <- readRDS(paths["evidence"])
routing <- terra::rast(paths["routing"])
fill <- terra::rast(paths["fill"])
direction <- terra::rast(paths["direction"])
accumulation <- terra::rast(paths["accumulation"])

if(!all(terra::compareGeom(routing,fill,direction,accumulation,stopOnError=FALSE)))
  stop("Routing outputs do not share one grid.")

collect_cells <- function(rasters,keep) {
  x <- if(inherits(rasters,"SpatRaster")) rasters else terra::rast(rasters)
  plan <- terra::blocks(x,n=32)
  terra::readStart(x)
  on.exit(terra::readStop(x),add=TRUE)
  pieces <- vector("list",plan$n)
  for(i in seq_len(plan$n)) {
    values <- terra::readValues(x,row=plan$row[i],nrows=plan$nrows[i],mat=TRUE)
    selected <- keep(values)
    if(any(selected)) {
      offset <- (plan$row[i]-1)*terra::ncol(x)
      cells <- offset+which(selected)
      pieces[[i]] <- data.frame(cell=cells,terra::xyFromCell(x,cells),
        values[selected,,drop=FALSE],check.names=FALSE)
    }
  }
  do.call(rbind,pieces)
}

filled <- collect_cells(fill,function(x) !is.na(x[,1]) & x[,1]>0)
names(filled)[4] <- "fill_depth"
routed <- collect_cells(c(direction,accumulation),function(x)
  !is.na(x[,1]) & x[,1]>0 & !is.na(x[,2]) & x[,2]>=threshold)
names(routed)[4:5] <- c("direction","accumulation")
unresolved <- collect_cells(direction,function(x) !is.na(x[,1]) & x[,1]==0)

dx <- c(`1`=1,`2`=1,`4`=0,`8`=-1,`16`=-1,`32`=-1,`64`=0,`128`=1)
dy <- c(`1`=0,`2`=-1,`4`=-1,`8`=-1,`16`=0,`32`=1,`64`=1,`128`=1)
res <- terra::res(routing)
routed$xend <- routed$x+unname(dx[as.character(routed$direction)])*res[1]
routed$yend <- routed$y+unname(dy[as.character(routed$direction)])*res[2]
routed <- routed[is.finite(routed$xend) & is.finite(routed$yend),]

pit_frequency <- evidence$pit_frequency
pit_cells <- sum(pit_frequency$count[pit_frequency$value>0])
pit_zones <- if(any(pit_frequency$value>0)) max(pit_frequency$value) else 0
zero_directions <- sum(evidence$direction_frequency$count[
  evidence$direction_frequency$value==0])
valid_cells <- result$preflight$valid_cells

map_extent <- terra::ext(routing)
legend_x <- terra::xmin(map_extent)+.43*(terra::xmax(map_extent)-terra::xmin(map_extent))
legend_y <- terra::ymax(map_extent)-.04*(terra::ymax(map_extent)-terra::ymin(map_extent))

png(file.path(result_dir,"pit-filled-pixels.png"),width=1800,height=1600,res=180,bg="white")
par(mar=c(7.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main="Priority-Flood changed pixels - Spencer Creek Hydro DEM",
  xlab="",ylab="Northing (m)")
fill_breaks <- unique(stats::quantile(filled$fill_depth,seq(0,1,length.out=7),type=7))
fill_group <- cut(filled$fill_depth,breaks=fill_breaks,include.lowest=TRUE,dig.lab=4)
fill_colors <- rev(hcl.colors(max(1,nlevels(fill_group)),"YlOrRd"))
points(filled$x,filled$y,pch=15,cex=.22,col=fill_colors[fill_group])
legend(legend_x,legend_y,legend=levels(fill_group),col=fill_colors,pch=15,pt.cex=.8,
  title="Fill depth (ft)",bg="white",cex=.78,xjust=0,yjust=1)
mtext(sprintf("%s cells changed (%.3f%% of valid terrain); maximum fill %.3f ft",
  format(nrow(filled),big.mark=","),100*nrow(filled)/valid_cells,result$maximum_fill),
  side=1,line=6.1,cex=.8)
dev.off()

png(file.path(result_dir,sprintf("derived-stream-threshold-%s.png",format(threshold,scientific=FALSE))),
  width=1800,height=1600,res=180,bg="white")
par(mar=c(7.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main=sprintf("Diagnostic flow paths - accumulation threshold %s cells",
    format(threshold,big.mark=",")),xlab="",ylab="Northing (m)")
segments(routed$x,routed$y,routed$xend,routed$yend,col="#0072B2",lwd=.45)
points(unresolved$x,unresolved$y,pch=15,cex=.10,col=grDevices::adjustcolor("#D55E00",.7))
legend(legend_x,legend_y,legend=c("Thresholded D8 segment","Unrouted flat/pit cell"),
  col=c("#0072B2","#D55E00"),lty=c(1,NA),lwd=c(2,NA),pch=c(NA,15),
  pt.cex=c(NA,.8),bg="white",cex=.85,xjust=0,yjust=1)
mtext(sprintf("DIAGNOSTIC ONLY - %s unrouted cells; %s interior pit zones; maximum accumulation %s cells",
  format(zero_directions,big.mark=","),format(pit_zones,big.mark=","),
  format(max(evidence$accumulation_range),big.mark=",")),side=1,line=6.1,cex=.8,col="#9C2F00")
dev.off()

summary <- data.frame(
  accumulation_threshold_cells=threshold,
  thresholded_segments=nrow(routed),
  filled_cells=nrow(filled),
  filled_percent_valid=100*nrow(filled)/valid_cells,
  maximum_fill_ft=result$maximum_fill,
  fill_sum_ft_m2=if(!is.null(result$fill_integral)) result$fill_integral else result$fill_volume,
  unrouted_cells=zero_directions,
  interior_pit_cells=pit_cells,
  interior_pit_zones=pit_zones,
  maximum_accumulation_cells=max(evidence$accumulation_range),
  flow_direction_seconds=evidence$flow_seconds,
  pitfinder_seconds=evidence$pit_seconds,
  flow_accumulation_seconds=evidence$accumulation_seconds,
  stringsAsFactors=FALSE
)
utils::write.csv(summary,file.path(result_dir,"routing-review-summary.csv"),row.names=FALSE)
print(summary)
