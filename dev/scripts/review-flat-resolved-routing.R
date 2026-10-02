# Create a first vector candidate and review map from the resolved flat directions.
# Usage: Rscript --vanilla dev/scripts/review-flat-resolved-routing.R RESULT_DIR THRESHOLD

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply the completed result directory and accumulation threshold (cells).")
result_dir <- normalizePath(args[1],mustWork=TRUE)
threshold <- as.numeric(args[2])
if(!is.finite(threshold) || threshold<1 || threshold!=floor(threshold))
  stop("The accumulation threshold must be a positive whole number of cells.")
paths <- file.path(result_dir,c("routing.tif","flow-direction-flat-resolved.tif",
  "flow-accumulation-flat-resolved.tif","flat-resolution-result.rds","flat-routing-evidence.rds"))
if(any(!file.exists(paths))) stop("The flat-resolution result is incomplete.")
routing <- terra::rast(paths[1])
direction <- terra::rast(paths[2])
accumulation <- terra::rast(paths[3])
resolution <- readRDS(paths[4])
evidence <- readRDS(paths[5])
if(!all(terra::compareGeom(routing,direction,accumulation,stopOnError=FALSE)))
  stop("Routing outputs do not share one grid.")

x <- c(direction,accumulation)
plan <- terra::blocks(x,n=32)
terra::readStart(x)
pieces <- vector("list",plan$n)
for(i in seq_len(plan$n)) {
  values <- terra::readValues(x,row=plan$row[i],nrows=plan$nrows[i],mat=TRUE)
  selected <- !is.na(values[,1]) & values[,1]>0 & !is.na(values[,2]) & values[,2]>=threshold
  if(any(selected)) {
    cells <- (plan$row[i]-1)*terra::ncol(x)+which(selected)
    pieces[[i]] <- data.frame(cell=cells,terra::xyFromCell(x,cells),
      direction=values[selected,1],accumulation=values[selected,2])
  }
}
terra::readStop(x)
stream <- do.call(rbind,pieces)
dx <- c(`1`=1,`2`=1,`4`=0,`8`=-1,`16`=-1,`32`=-1,`64`=0,`128`=1)
dy <- c(`1`=0,`2`=-1,`4`=-1,`8`=-1,`16`=0,`32`=1,`64`=1,`128`=1)
res <- terra::res(routing)
stream$xend <- stream$x+unname(dx[as.character(stream$direction)])*res[1]
stream$yend <- stream$y+unname(dy[as.character(stream$direction)])*res[2]
stream <- stream[is.finite(stream$xend) & is.finite(stream$yend),]

geometries <- lapply(seq_len(nrow(stream)),function(i)
  sf::st_linestring(matrix(c(stream$x[i],stream$y[i],stream$xend[i],stream$yend[i]),
    ncol=2,byrow=TRUE)))
candidate <- sf::st_sf(cell=stream$cell,accumulation_cells=stream$accumulation,
  geometry=sf::st_sfc(geometries,crs=sf::st_crs(terra::crs(routing))))
gpkg <- file.path(result_dir,sprintf("stream-network-threshold-%s.gpkg",threshold))
if(file.exists(gpkg)) stop("Candidate GeoPackage already exists; retain or remove it explicitly.")
sf::st_write(candidate,gpkg,"stream_network",quiet=TRUE)

outlet_xy <- terra::xyFromCell(routing,resolution$outlet_cells)
extent <- terra::ext(routing)
legend_x <- terra::xmin(extent)+.43*(terra::xmax(extent)-terra::xmin(extent))
legend_y <- terra::ymax(extent)-.04*(terra::ymax(extent)-terra::ymin(extent))
figure <- file.path(result_dir,sprintf("derived-stream-flat-resolved-threshold-%s.png",threshold))
png(figure,width=1800,height=1600,res=180,bg="white")
par(mar=c(7.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,
  main=sprintf("Flat-resolved flow paths - accumulation threshold %s cells",
    format(threshold,big.mark=",")),xlab="",ylab="Northing (m)")
segments(stream$x,stream$y,stream$xend,stream$yend,col="#0072B2",lwd=.48)
points(outlet_xy[,1],outlet_xy[,2],pch=23,bg="#D55E00",col="white",cex=1.6)
legend(legend_x,legend_y,legend=c("Thresholded D8 segment","Reviewed outlet"),
  col=c("#0072B2","white"),pt.bg=c(NA,"#D55E00"),lty=c(1,NA),lwd=c(2,NA),
  pch=c(NA,23),pt.cex=c(NA,1.2),bg="white",cex=.85,xjust=0,yjust=1)
mtext(sprintf("All %s valid cells accumulate to the outlet; %s flat cells resolved; maximum accumulation %s cells",
  format(resolution$preflight$valid_cells,big.mark=","),
  format(resolution$resolution$resolved_cells,big.mark=","),
  format(max(evidence$accumulation_range),big.mark=",")),side=1,line=6.1,cex=.8)
dev.off()

summary <- data.frame(
  accumulation_threshold_cells=threshold,
  vector_segments=nrow(candidate),
  flat_labels=resolution$resolution$flat_labels,
  resolved_flat_cells=resolution$resolution$resolved_cells,
  unresolved_non_outlet_cells=resolution$resolution$unresolved_cells,
  maximum_flat_mask=resolution$resolution$maximum_mask,
  flat_resolution_seconds=resolution$resolution$elapsed_seconds,
  maximum_accumulation_cells=max(evidence$accumulation_range),
  outlet_accumulation_cells=evidence$outlet_accumulation,
  accumulation_seconds=evidence$accumulation_seconds
)
utils::write.csv(summary,file.path(result_dir,"flat-resolved-routing-summary.csv"),row.names=FALSE)
print(summary)
