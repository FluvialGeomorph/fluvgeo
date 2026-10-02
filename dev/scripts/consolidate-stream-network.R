# Consolidate thresholded D8 cell edges into maximal lines between network nodes.
# Usage: Rscript --vanilla dev/scripts/consolidate-stream-network.R RESULT_DIR THRESHOLD

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply the completed result directory and threshold (cells).")
result_dir <- normalizePath(args[1],mustWork=TRUE)
threshold <- as.numeric(args[2])
if(!is.finite(threshold) || threshold<100 || threshold!=floor(threshold))
  stop("Threshold must be a whole number of at least 100 cells.")
routing <- terra::rast(file.path(result_dir,"routing.tif"))
source_path <- file.path(result_dir,"stream-network-threshold-100.gpkg")
if(!file.exists(source_path)) stop("The complete 100-cell candidate is required.")
edges <- sf::st_read(source_path,"stream_network",quiet=TRUE)
edges <- edges[edges$accumulation_cells>=threshold,,drop=FALSE]
if(!nrow(edges)) stop("No network edges meet the threshold.")

coordinates <- sf::st_coordinates(edges)
first <- !duplicated(coordinates[,"L1"])
last <- !duplicated(coordinates[,"L1"],fromLast=TRUE)
start_xy <- coordinates[first,c("X","Y"),drop=FALSE]
end_xy <- coordinates[last,c("X","Y"),drop=FALSE]
downstream_cell <- terra::cellFromXY(routing,end_xy)
next_edge <- match(downstream_cell,edges$cell)
indegree <- tabulate(next_edge[!is.na(next_edge)],nbins=nrow(edges))
starts <- which(indegree!=1L)
used <- rep(FALSE,nrow(edges))
records <- vector("list",length(starts))
geometries <- vector("list",length(starts))
reach <- 0L

for(seed in starts) {
  if(used[seed]) next
  current <- seed
  edge_ids <- integer()
  xy <- matrix(start_xy[current,],nrow=1)
  repeat {
    if(used[current]) stop("Network consolidation encountered a repeated edge.")
    used[current] <- TRUE
    edge_ids <- c(edge_ids,current)
    xy <- rbind(xy,end_xy[current,])
    next_id <- next_edge[current]
    if(is.na(next_id) || indegree[next_id]!=1L) break
    current <- next_id
  }
  reach <- reach+1L
  geometries[[reach]] <- sf::st_linestring(xy)
  records[[reach]] <- data.frame(
    reach_id=sprintf("SN%05d",reach),
    upstream_cell=edges$cell[edge_ids[1]],
    downstream_cell=downstream_cell[edge_ids[length(edge_ids)]],
    segment_count=length(edge_ids),
    upstream_accumulation_cells=edges$accumulation_cells[edge_ids[1]],
    downstream_accumulation_cells=edges$accumulation_cells[edge_ids[length(edge_ids)]],
    length_m=sum(sqrt(rowSums((end_xy[edge_ids,,drop=FALSE]-start_xy[edge_ids,,drop=FALSE])^2)))
  )
}
if(any(!used)) stop("Thresholded network contains a cycle or unreachable edge.")
records <- do.call(rbind,records[seq_len(reach)])
network <- sf::st_sf(records,
  geometry=sf::st_sfc(geometries[seq_len(reach)],crs=sf::st_crs(edges)))
output <- file.path(result_dir,sprintf("stream-network-threshold-%s-consolidated.gpkg",threshold))
if(file.exists(output)) stop("Consolidated candidate already exists; retain or remove it explicitly.")
sf::st_write(network,output,"stream_network",quiet=TRUE)

junctions <- which(indegree>1L)
figure <- file.path(result_dir,sprintf("stream-network-threshold-%s-consolidated.png",threshold))
extent <- terra::ext(routing)
legend_x <- terra::xmin(extent)+.43*(terra::xmax(extent)-terra::xmin(extent))
legend_y <- terra::ymax(extent)-.04*(terra::ymax(extent)-terra::ymin(extent))
png(figure,width=1800,height=1600,res=180,bg="white")
par(mar=c(7.5,4.5,3.2,1),las=1)
terra::plot(routing,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,
  main=sprintf("Consolidated stream network - threshold %s cells (%.3g ha)",
    format(threshold,big.mark=","),threshold*prod(terra::res(routing))/10000),
  xlab="",ylab="Northing (m)")
plot(sf::st_geometry(network),add=TRUE,col="#0072B2",lwd=.75)
if(length(junctions)) points(start_xy[junctions,1],start_xy[junctions,2],
  pch=21,bg="#E69F00",col="white",cex=.65)
legend(legend_x,legend_y,legend=c("Consolidated D8 network","Junction"),
  col=c("#0072B2","white"),pt.bg=c(NA,"#E69F00"),lty=c(1,NA),lwd=c(2,NA),
  pch=c(NA,21),pt.cex=c(NA,1),bg="white",cex=.85,xjust=0,yjust=1)
mtext(sprintf("%s cell edges consolidated to %s lines between heads, junctions and outlet; total length %.2f km",
  format(nrow(edges),big.mark=","),format(nrow(network),big.mark=","),
  sum(network$length_m)/1000),side=1,outer=FALSE,line=6.1,cex=.8)
dev.off()

summary <- data.frame(
  threshold_cells=threshold,
  contributing_area_ha=threshold*prod(terra::res(routing))/10000,
  input_cell_segments=nrow(edges),
  consolidated_lines=nrow(network),
  headwater_nodes=sum(indegree==0L),
  junction_nodes=length(junctions),
  total_length_m=sum(network$length_m),
  all_input_edges_used=all(used),
  all_geometries_valid=all(sf::st_is_valid(network))
)
utils::write.csv(summary,file.path(result_dir,sprintf("stream-network-threshold-%s-consolidated-summary.csv",threshold)),row.names=FALSE)
print(summary)
