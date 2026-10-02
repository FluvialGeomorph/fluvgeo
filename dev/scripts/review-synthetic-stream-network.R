# Create the two required analyst-review figures from the public extraction API.
# Usage: Rscript --vanilla dev/scripts/review-synthetic-stream-network.R RESULT_DIR

args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply one completed extraction directory.")
directory <- normalizePath(args[1],mustWork=TRUE)
result <- readRDS(file.path(directory,"result.rds"))
hydro <- terra::rast(result$source)
fill <- terra::rast(file.path(directory,result$files$fill_depth))
network <- sf::st_read(file.path(directory,result$files$stream_network),quiet=TRUE)

blocks <- terra::blocks(fill,n=32);pieces <- vector("list",blocks$n)
terra::readStart(fill)
for(i in seq_len(blocks$n)) {
  value <- terra::readValues(fill,row=blocks$row[i],nrows=blocks$nrows[i],mat=FALSE)
  keep <- is.finite(value) & value>0
  if(any(keep)) {
    cell <- (blocks$row[i]-1)*terra::ncol(fill)+which(keep)
    pieces[[i]] <- data.frame(terra::xyFromCell(fill,cell),depth=value[keep])
  }
}
terra::readStop(fill)
filled <- do.call(rbind,pieces)
breaks <- unique(stats::quantile(filled$depth,seq(0,1,length.out=7),type=7))
group <- cut(filled$depth,breaks=breaks,include.lowest=TRUE,dig.lab=4)
colors <- rev(grDevices::hcl.colors(max(1,nlevels(group)),"YlOrRd"))

png(file.path(directory,"pit-filled-pixels.png"),width=1800,height=1600,res=180,bg="white")
par(mar=c(6.5,4.5,3.2,1),las=1)
terra::plot(hydro,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main="Routing fill changes over the Hydro DEM",xlab="",ylab="Northing (m)")
points(filled$x,filled$y,pch=15,cex=.22,col=colors[group])
legend("topright",legend=levels(group),col=colors,pch=15,pt.cex=.8,
  title="Fill depth (ft)",bg="white",cex=.78)
mtext(sprintf("%s changed cells; maximum fill %.3f ft",format(nrow(filled),big.mark=","),
  max(filled$depth)),side=1,line=5.2,cex=.8)
dev.off()

png(file.path(directory,"derived-stream-network-1ha.png"),width=1800,height=1600,res=180,bg="white")
par(mar=c(6.5,4.5,3.2,1),las=1)
terra::plot(hydro,col=hcl.colors(128,"Grays",rev=TRUE),maxcell=600000,
  axes=TRUE,legend=FALSE,main=sprintf("Terrain-derived stream network - %.3g ha",result$threshold_ha),
  xlab="",ylab="Northing (m)")
plot(sf::st_geometry(network),add=TRUE,col="#0072B2",lwd=.8)
mtext(sprintf("%s lines; %s m; outlet cell %s",format(nrow(network),big.mark=","),
  format(round(sum(network$length_m)),big.mark=","),format(result$outlet_cell,big.mark=",")),
  side=1,line=5.2,cex=.8)
dev.off()
