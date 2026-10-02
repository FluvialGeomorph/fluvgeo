# Compare accumulation thresholds without rerunning fill, direction or accumulation.
# Usage: Rscript --vanilla dev/scripts/compare-stream-thresholds.R RESULT_DIR

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply the completed result directory.")
result_dir <- normalizePath(args[1],mustWork=TRUE)
routing_path <- file.path(result_dir,"routing.tif")
candidate_path <- file.path(result_dir,"stream-network-threshold-100.gpkg")
if(!file.exists(routing_path) || !file.exists(candidate_path))
  stop("The completed 100-cell routing candidate is required.")
routing <- terra::rast(routing_path)
candidate <- sf::st_read(candidate_path,"stream_network",quiet=TRUE)
if(nrow(candidate)==0 || !"accumulation_cells" %in% names(candidate))
  stop("The candidate does not contain accumulated D8 segments.")
thresholds <- c(100,500,1000,2500,5000,10000)
if(any(terra::res(routing)<=0)) stop("Routing grid resolution is invalid.")
cell_area <- prod(terra::res(routing))

coordinates <- sf::st_coordinates(candidate)
first <- !duplicated(coordinates[,"L1"])
last <- !duplicated(coordinates[,"L1"],fromLast=TRUE)
lines <- data.frame(
  x=coordinates[first,"X"],y=coordinates[first,"Y"],
  xend=coordinates[last,"X"],yend=coordinates[last,"Y"],
  accumulation=candidate$accumulation_cells
)
counts <- vapply(thresholds,function(value) sum(lines$accumulation>=value),numeric(1))
summary <- data.frame(
  threshold_cells=thresholds,
  contributing_area_m2=thresholds*cell_area,
  contributing_area_ha=thresholds*cell_area/10000,
  retained_segments=counts,
  retained_percent_of_100_cell=100*counts/nrow(lines)
)
utils::write.csv(summary,file.path(result_dir,"stream-threshold-comparison.csv"),row.names=FALSE)

figure <- file.path(result_dir,"stream-threshold-comparison.png")
png(figure,width=2400,height=1800,res=180,bg="white")
par(mfrow=c(2,3),mar=c(3.4,3.5,3.4,.8),oma=c(2.8,1,3.2,1),las=1)
for(i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  terra::plot(routing,col=hcl.colors(96,"Grays",rev=TRUE),maxcell=180000,
    axes=TRUE,legend=FALSE,xlab="",ylab="",
    main=sprintf("%s cells (%.3g ha)",format(threshold,big.mark=","),
      summary$contributing_area_ha[i]))
  selected <- lines$accumulation>=threshold
  segments(lines$x[selected],lines$y[selected],lines$xend[selected],lines$yend[selected],
    col="#0072B2",lwd=.38)
  mtext(sprintf("%s segments",format(counts[i],big.mark=",")),side=1,line=2.25,cex=.72)
}
mtext("Spencer Creek flat-resolved stream-initiation threshold comparison",
  side=3,outer=TRUE,line=1.2,cex=1.25,font=2)
mtext("Threshold is accumulated 1 m cells; routing and terrain are identical in every panel",
  side=1,outer=TRUE,line=1.2,cex=.82)
dev.off()
print(summary)
