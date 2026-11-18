# GEOS constrained triangulation supplies a navigation mesh inside a polygon.
# Portal midpoints keep graph edges inside triangles. Visibility string-pulling
# removes unnecessary vertices; bends have no prescribed centerline location.
cross_section_route_leg <- function(bank,target,polygon,outer_boundary,obstacles,tolerance,fixed_target=FALSE) {
  crs<-sf::st_crs(polygon)
  point<-sf::st_sfc(sf::st_point(bank),crs=crs)
  safe<-polygon
  if(length(obstacles))safe<-suppressWarnings(sf::st_difference(polygon,
    sf::st_union(sf::st_buffer(obstacles,tolerance*4))))
  if(!length(safe) || all(sf::st_is_empty(safe)))return(NULL)
  safe<-suppressWarnings(sf::st_collection_extract(safe,"POLYGON"))
  safe<-suppressWarnings(sf::st_cast(sf::st_cast(safe,"MULTIPOLYGON"),"POLYGON"))
  touched<-sf::st_is_within_distance(point,safe,dist=tolerance/1000)[[1]]
  if(!length(touched))return(NULL)
  best<-NULL;best_length<-Inf
  for(part in touched) {
    cell<-safe[part]
    if(fixed_target) {
      endpoint<-target
      endpoint_point<-sf::st_sfc(sf::st_point(endpoint),crs=crs)
      if(!length(sf::st_is_within_distance(endpoint_point,cell,dist=tolerance/1000)[[1]]) ||
          as.numeric(sf::st_distance(endpoint_point,outer_boundary))>tolerance/1000)next
    } else {
      outer<-suppressWarnings(sf::st_intersection(sf::st_boundary(cell),outer_boundary))
      if(!length(outer) || all(sf::st_is_empty(outer)))next
      endpoint<-sf::st_coordinates(sf::st_nearest_points(sf::st_sfc(sf::st_point(target),crs=crs),
        sf::st_union(outer)))[2,1:2]
    }
    if(sqrt(sum((endpoint-bank)^2))<=tolerance)next
    line<-sf::st_sfc(sf::st_linestring(rbind(bank,endpoint)),crs=crs)
    if(sf::st_covered_by(line,cell,sparse=FALSE)[1,1]) {
      route<-line
    } else {
      triangles<-suppressWarnings(sf::st_collection_extract(sf::st_triangulate_constrained(cell),"POLYGON"))
      triangles<-suppressWarnings(sf::st_cast(triangles,"POLYGON"))
      if(!length(triangles))next
      centers<-sf::st_coordinates(suppressWarnings(sf::st_centroid(triangles)))[,1:2,drop=FALSE]
      # GEOS triangles have exact shared edges. Index those edges directly,
      # avoiding one expensive overlay call for every neighboring triangle.
      vertices<-sf::st_coordinates(triangles)[,1:2,drop=FALSE]
      if(nrow(vertices)!=4L*length(triangles))next
      keys<-paste(sprintf("%.17g",vertices[,1]),sprintf("%.17g",vertices[,2]),sep=":")
      unique_vertices<-vertices[!duplicated(keys),,drop=FALSE]
      ids<-matrix(match(keys,unique(keys)),ncol=4L,byrow=TRUE)[,1:3,drop=FALSE]
      a<-c(ids[,1],ids[,2],ids[,3]);b<-c(ids[,2],ids[,3],ids[,1])
      owners<-rep(seq_len(nrow(ids)),3L)
      edge_keys<-paste(pmin(a,b),pmax(a,b),sep=":")
      second<-which(duplicated(edge_keys));first<-match(edge_keys[second],edge_keys)
      portals<-(unique_vertices[a[second],,drop=FALSE]+unique_vertices[b[second],,drop=FALSE])/2
      portal_ids<-nrow(centers)+seq_along(second)
      coordinates<-rbind(centers,portals)
      edges<-rbind(cbind(owners[first],portal_ids),cbind(owners[second],portal_ids))
      endpoint_point<-sf::st_sfc(sf::st_point(endpoint),crs=crs)
      starts<-sf::st_is_within_distance(point,triangles,dist=tolerance/1000)[[1]]
      ends<-sf::st_is_within_distance(endpoint_point,triangles,dist=tolerance/1000)[[1]]
      if(!length(starts) || !length(ends))next
      coordinates<-rbind(coordinates,bank,endpoint);n<-nrow(coordinates)
      edges<-rbind(edges,cbind(starts,n-1L),cbind(ends,n))
      graph<-igraph::make_empty_graph(n=n,directed=FALSE)
      graph<-igraph::add_edges(graph,as.vector(t(edges)))
      weights<-sqrt(rowSums((coordinates[edges[,1],,drop=FALSE]-coordinates[edges[,2],,drop=FALSE])^2))
      path<-suppressWarnings(igraph::shortest_paths(graph,from=n-1L,to=n,weights=weights)$vpath[[1]])
      if(!length(path))next
      xy<-coordinates[as.integer(path),,drop=FALSE]
      selected<-1L;anchor<-1L
      margin<-sf::st_buffer(cell,tolerance/1000,nQuadSegs=1)
      # GEOS line-of-sight tests are batched for each retained bend.
      while(anchor<nrow(xy)) {
        destinations<-(anchor+1L):nrow(xy)
        rays<-sf::st_sfc(lapply(destinations,function(j)sf::st_linestring(rbind(xy[anchor,],xy[j,]))),crs=crs)
        visible<-sf::st_covered_by(rays,margin,sparse=FALSE)[,1]
        if(!any(visible))break
        anchor<-max(destinations[visible]);selected<-c(selected,anchor)
      }
      if(tail(selected,1)!=nrow(xy))next
      route<-sf::st_sfc(sf::st_linestring(xy[selected,,drop=FALSE]),crs=crs)
    }
    if(!sf::st_is_simple(route) || (length(obstacles) && length(sf::st_intersects(route,obstacles)[[1]])))next
    if(sum(as.numeric(sf::st_length(sf::st_difference(route,polygon))))>tolerance)next
    distance<-as.numeric(sf::st_length(route))
    if(distance<best_length) {best<-route;best_length<-distance}
  }
  best
}

