# Create required review maps and compare native D8 edges with an accepted network.
# Usage: Rscript --vanilla dev/scripts/compare-native-d8-stream.R RESULT_DIR REFERENCE_GPKG

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply the native result directory and accepted network GeoPackage.")
result_dir <- normalizePath(args[1],mustWork=TRUE)
reference_path <- normalizePath(args[2],mustWork=TRUE)
routing <- terra::rast(file.path(result_dir,"routing.tif"))
fill <- terra::rast(file.path(result_dir,"fill-depth.tif"))
edges <- sf::st_read(file.path(result_dir,"stream-network-threshold-100.gpkg"),
  "stream_network",quiet=TRUE)
edges <- edges[edges$accumulation_cells>=10000,,drop=FALSE]
network <- sf::st_read(file.path(result_dir,
  "stream-network-threshold-10000-consolidated.gpkg"),"stream_network",quiet=TRUE)
reference <- sf::st_read(reference_path,quiet=TRUE)

collect_positive <- function(x) {
  plan <- terra::blocks(x,n=32)
  terra::readStart(x);on.exit(terra::readStop(x),add=TRUE)
  pieces <- vector("list",plan$n)
  for(i in seq_len(plan$n)) {
    values <- terra::readValues(x,row=plan$row[i],nrows=plan$nrows[i],mat=FALSE)
    keep <- is.finite(values) & values>0
    if(any(keep)) {
      cells <- (plan$row[i]-1)*terra::ncol(x)+which(keep)
      pieces[[i]] <- data.frame(cell=cells,terra::xyFromCell(x,cells),depth=values[keep])
    }
  }
  do.call(rbind,pieces)
}

filled <- collect_positive(fill)
breaks <- unique(stats::quantile(filled$depth,seq(0,1,length.out=7),type=7))
groups <- cut(filled$depth,breaks=breaks,include.lowest=TRUE,dig.lab=4)
colors <- rev(hcl.colors(max(1,nlevels(groups)),"YlOrRd"))
png(file.path(result_dir,"pit-filled-pixels.png"),width=1800,height=1600,res=180,bg="white")
par(mar=c(6.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main="Priority-Flood changed pixels - Spencer Creek Hydro DEM",
  xlab="",ylab="Northing (m)")
points(filled$x,filled$y,pch=15,cex=.22,col=colors[groups])
legend("topright",legend=levels(groups),col=colors,pch=15,pt.cex=.8,
  title="Fill depth (ft)",bg="white",cex=.78)
mtext(sprintf("%s cells changed (%.3f%% of valid terrain); maximum fill %.3f ft",
  format(nrow(filled),big.mark=","),100*nrow(filled)/terra::global(!is.na(routing),"sum")[1,1],
  max(filled$depth)),side=1,line=5.2,cex=.8)
dev.off()

reference_segments <- lapply(seq_len(nrow(reference)),function(i) {
  xy <- sf::st_coordinates(sf::st_geometry(reference)[[i]])[,c("X","Y"),drop=FALSE]
  data.frame(x=xy[-nrow(xy),1],y=xy[-nrow(xy),2],
    xend=xy[-1,1],yend=xy[-1,2])
})
reference_segments <- do.call(rbind,reference_segments)
reference_segments$cell <- terra::cellFromXY(routing,reference_segments[,c("x","y")])
new_cells <- edges$cell
reference_cells <- reference_segments$cell
common <- intersect(new_cells,reference_cells)
new_only <- setdiff(new_cells,reference_cells)
reference_only <- setdiff(reference_cells,new_cells)

new_only_edges <- edges[match(new_only,edges$cell),,drop=FALSE]
reference_only_segments <- reference_segments[match(reference_only,reference_segments$cell),,drop=FALSE]
png(file.path(result_dir,"derived-stream-comparison-1ha.png"),
  width=1800,height=1600,res=180,bg="white")
par(mar=c(6.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main="Native D8 stream network compared with reviewed result - 1 ha",
  xlab="",ylab="Northing (m)")
plot(sf::st_geometry(reference),add=TRUE,col="#666666",lwd=1.7)
plot(sf::st_geometry(network),add=TRUE,col="#0072B2",lwd=.8)
if(nrow(reference_only_segments)) with(reference_only_segments,
  segments(x,y,xend,yend,col="#D55E00",lwd=1.3))
if(nrow(new_only_edges)) plot(sf::st_geometry(new_only_edges),add=TRUE,
  col="#CC79A7",lwd=1.3)
map_extent <- terra::ext(routing)
legend_x <- terra::xmin(map_extent)+.43*(terra::xmax(map_extent)-terra::xmin(map_extent))
legend_y <- terra::ymax(map_extent)-.04*(terra::ymax(map_extent)-terra::ymin(map_extent))
legend(legend_x,legend_y,legend=c("Overlap","Reviewed only","Native only"),
  col=c("#0072B2","#D55E00","#CC79A7"),lty=1,lwd=c(2,2,2),bg="white",cex=.82,
  xjust=0,yjust=1)
mtext(sprintf("%s of %s unique D8 edges agree (Jaccard %.3f); native %s edges / %s lines; reviewed %s edges / %s lines",
  format(length(common),big.mark=","),format(length(union(new_cells,reference_cells)),big.mark=","),
  length(common)/length(union(new_cells,reference_cells)),format(length(new_cells),big.mark=","),
  nrow(network),format(length(reference_cells),big.mark=","),nrow(reference)),
  side=1,line=5.2,cex=.75)
dev.off()

summary <- data.frame(
  threshold_cells=10000,threshold_area_ha=10000*prod(terra::res(routing))/10000,
  native_edges=length(new_cells),reviewed_edges=length(reference_cells),
  common_edges=length(common),native_only_edges=length(new_only),
  reviewed_only_edges=length(reference_only),
  edge_jaccard=length(common)/length(union(new_cells,reference_cells)),
  native_lines=nrow(network),reviewed_lines=nrow(reference),
  native_length_m=sum(network$length_m),reviewed_length_m=sum(reference$length_m),
  filled_cells=nrow(filled),maximum_fill=max(filled$depth)
)
utils::write.csv(summary,file.path(result_dir,"native-d8-comparison-summary.csv"),row.names=FALSE)
print(summary)
