# Boundary chainage is topological order along an actual shared polygon edge,
# not a nearest-centerline projection of every anchor (which folds at bends).
# Angular floodplain alternatives are proposals only. Reduce contacts before
# assigning their outer positions in station order; final routing enforces rank.
cross_section_proposal_choice <- function(geometry,groups,angles) {
  hits <- sf::st_intersects(geometry)
  selected <- vapply(groups,function(g)if(length(g))g[1] else NA_integer_,integer(1))
  better <- function(a,b) {d<-which(abs(a-b)>1e-9);length(d)>0L && a[d[1]]<b[d[1]]}
  cost <- function(candidates,others) {
    others <- others[!is.na(others)]
    n <- sum(vapply(candidates,function(j)sum(others %in% hits[[j]]),integer(1)))
    if(length(candidates)==2L)n <- n+as.integer(candidates[2] %in% hits[[candidates[1]]])
    c(n,sum(abs(angles[candidates])))
  }
  repeat {
    changed <- FALSE
    for(g in which(!is.na(selected))) {
      others <- selected[-g];active<-rep(FALSE,length(geometry));active[others[!is.na(others)]]<-TRUE
      candidates <- groups[[g]]
      counts <- vapply(candidates,function(j)sum(active[hits[[j]]]),integer(1))
      j <- order(counts,abs(angles[candidates]))[1];best <- candidates[j]
      if(better(c(counts[j],abs(angles[best])),cost(selected[g],others)) && best!=selected[g]) {
        selected[g] <- best;changed <- TRUE
      }
    }
    pairs <- which(!is.na(selected))
    for(g in pairs)for(h in pairs[pairs>g]) {
      if(!selected[h] %in% hits[[selected[g]]])next
      others <- selected[-c(g,h)];active<-rep(FALSE,length(geometry));active[others[!is.na(others)]]<-TRUE
      a <- groups[[g]];b <- groups[[h]]
      ac <- vapply(a,function(j)sum(active[hits[[j]]]),integer(1))
      bc <- vapply(b,function(j)sum(active[hits[[j]]]),integer(1))
      counts <- outer(ac,bc,"+")+do.call(rbind,lapply(a,function(j)as.integer(b %in% hits[[j]])))
      angles_cost <- outer(abs(angles[a]),abs(angles[b]),"+")
      grid <- expand.grid(a=seq_along(a),b=seq_along(b))
      index <- order(as.vector(counts),as.vector(angles_cost),grid$a,grid$b)[1]
      if(better(c(counts[index],angles_cost[index]),cost(selected[c(g,h)],others))) {
        selected[c(g,h)] <- c(a[grid$a[index]],b[grid$b[index]]);changed <- TRUE
      }
    }
    if(!changed)break
  }
  selected
}

cross_section_boundary_chains <- function(regions, channel, floodplain, flowline, paths, tolerance, sections=NULL) {
  crs <- sf::st_crs(regions)
  boundaries <- list(bank=sf::st_boundary(sf::st_union(sf::st_geometry(channel))),
    outer=sf::st_boundary(sf::st_union(sf::st_geometry(floodplain))))
  rows <- list()
  for(i in seq_len(nrow(regions))) for(role in names(boundaries)) {
    edge <- suppressWarnings(sf::st_intersection(sf::st_boundary(sf::st_geometry(regions[i,])),boundaries[[role]]))
    edge <- edge[sf::st_geometry_type(edge) %in% c("LINESTRING","MULTILINESTRING","GEOMETRYCOLLECTION")]
    if(any(sf::st_geometry_type(edge)=="GEOMETRYCOLLECTION"))
      edge <- suppressWarnings(sf::st_collection_extract(edge,"LINESTRING"))
    if(!length(edge) || all(sf::st_is_empty(edge)))next
    edge <- suppressWarnings(sf::st_cast(sf::st_line_merge(
      sf::st_cast(sf::st_union(edge),"MULTILINESTRING")),"LINESTRING"))
    reference <- paths[paths$region_id==regions$region_id[i],,drop=FALSE]
    reference <- if(nrow(reference)==1L)sf::st_geometry(reference) else sf::st_geometry(flowline)
    for(j in seq_along(edge)) {
      xy <- sf::st_coordinates(edge[j])[,1:2,drop=FALSE]
      if(nrow(xy)<2L || as.numeric(sf::st_length(edge[j]))<=tolerance)next
      ends <- sf::st_sfc(sf::st_point(xy[1,]),sf::st_point(xy[nrow(xy),]),crs=crs)
      measures <- as.numeric(sf::st_line_project(reference,ends))
      oriented <- abs(diff(measures))>tolerance && sqrt(sum((xy[1,]-xy[nrow(xy),])^2))>tolerance
      if(diff(measures)<0)xy <- xy[nrow(xy):1,,drop=FALSE]
      rows[[length(rows)+1L]] <- sf::st_sf(chain_id=paste(regions$region_id[i],role,j,sep=":"),
        region_id=regions$region_id[i],role=role,oriented=oriented,
        geometry=sf::st_sfc(sf::st_linestring(xy),crs=crs))
    }
  }
  result <- if(length(rows))do.call(rbind,rows) else sf::st_sf(chain_id=character(),region_id=character(),
    role=character(),oriented=logical(),geometry=sf::st_sfc(crs=crs))
  if(!is.null(sections) && nrow(sections)>1L)result <- rbind(result,
    cross_section_channel_bank_chains(sections,channel,tolerance))
  result
}

# A whole channel bank must not lose its ordering check where adjacent
# floodplain components change. Select each bank arc on the exterior ring by
# its left/right anchor membership, never by a shortest-cross-section objective.
cross_section_channel_bank_chains <- function(sections,channel,tolerance) {
  crs <- sf::st_crs(sections)
  rings <- suppressWarnings(sf::st_cast(sf::st_boundary(sf::st_cast(
    sf::st_union(sf::st_geometry(channel)),"POLYGON")),"LINESTRING"))
  points <- lapply(c("left","right"),function(side)sf::st_sfc(lapply(seq_len(nrow(sections)),function(i) {
    xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
    sf::st_point(xy[sections[[paste0(side,"_bank_vertex")]][i],])
  }),crs=crs))
  parts <- lapply(points,sf::st_nearest_feature,y=rings)
  rows <- list()
  for(part in seq_along(rings))for(side in 1:2) {
    group <- which(parts[[side]]==part);group <- group[order(sections$Seq[group])]
    if(length(group)<2L)next
    ring <- rings[part];len <- as.numeric(sf::st_length(ring))
    m <- as.numeric(sf::st_line_project(ring,points[[side]][group]))
    other <- as.numeric(sf::st_line_project(ring,points[[3L-side]][parts[[3L-side]]==part]))
    start <- m[1];end <- tail(m,1);span <- (end-start+len)%%len
    inside <- function(values)(values-start+len)%%len<=span+tolerance
    forward <- sum(inside(m))-sum(inside(other))
    reverse <- sum(!inside(m[-c(1,length(m))]))+2L-sum(!inside(other))
    positive <- forward>=reverse
    a <- if(positive)start else end;b <- if(positive)end else start
    piece <- function(a,b)sf::st_coordinates(lwgeom::st_linesubstring(ring,a/len,b/len))[,1:2,drop=FALSE]
    xy <- if(span<=tolerance || span>=len-tolerance)sf::st_coordinates(ring)[,1:2,drop=FALSE] else if(a<=b)piece(a,b) else {
      first <- piece(a,len);last <- piece(0,b)
      rbind(first,last[-1,,drop=FALSE])
    }
    if(!positive)xy <- xy[nrow(xy):1,,drop=FALSE]
    rows[[length(rows)+1L]] <- sf::st_sf(chain_id=paste0("channel-",part,":",c("left","right")[side],"-bank"),
      region_id=paste0("channel-",part),role=paste0(c("left","right")[side],"_channel_bank"),
      oriented=forward!=reverse && span>tolerance && span<len-tolerance,
      geometry=sf::st_sfc(sf::st_linestring(xy),crs=crs))
  }
  if(length(rows))do.call(rbind,rows) else sf::st_sf(chain_id=character(),region_id=character(),
    role=character(),oriented=logical(),geometry=sf::st_sfc(crs=crs))
}

cross_section_channel_anchors <- function(sections,chains,tolerance) {
  crs <- sf::st_crs(sections);rows <- list()
  for(i in seq_len(nrow(sections)))for(side in c("left","right")) {
    xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
    point <- sf::st_sfc(sf::st_point(xy[sections[[paste0(side,"_bank_vertex")]][i],]),crs=crs)
    eligible <- which(chains$role==paste0(side,"_channel_bank"))
    chain <- if(length(eligible))eligible[sf::st_nearest_feature(point,chains[eligible,])] else NA_integer_
    measure <- NA_real_;status <- if(nrow(sections)==1L)"single_section" else "unavailable_channel_bank"
    if(!is.na(chain)) {
      measure <- as.numeric(sf::st_line_project(sf::st_geometry(chains[chain,]),point))
      status <- if(!chains$oriented[chain])"ambiguous_channel_bank" else
        if(as.numeric(sf::st_distance(point,chains[chain,]))>tolerance)"off_boundary" else "ordered"
    }
    rows[[length(rows)+1L]] <- sf::st_sf(candidate_id=sections$candidate_id[i],Seq=sections$Seq[i],side=side,
      role="channel_bank",region_id=if(is.na(chain))NA_character_ else chains$region_id[chain],
      chain_id=if(is.na(chain))NA_character_ else chains$chain_id[chain],
      proposed_measure=measure,measure=measure,status=status,geometry=point)
  }
  if(!length(rows))return(NULL)
  result <- do.call(rbind,rows)
  for(key in unique(stats::na.omit(result$chain_id))) {
    group <- which(result$chain_id==key & result$status=="ordered");group <- group[order(result$Seq[group])]
    reversed <- which(diff(result$measure[group])<=tolerance)
    if(length(reversed))result$status[group[unique(c(reversed,reversed+1L))]] <- "bank_order_conflict"
  }
  result$measure_units <- rep(crs$units_gdal,nrow(result));result
}

cross_section_order_anchors <- function(sections, chains, tolerance, assign=TRUE) {
  crs <- sf::st_crs(sections)
  rows <- list()
  for(i in seq_len(nrow(sections))) for(side in c("left","right")) {
    s <- sections[i,]
    if(is.na(s[[paste0(side,"_region_id")]]) || s[[paste0(side,"_leg_status")]]=="no_floodplain")next
    xy <- sf::st_coordinates(s)[,1:2,drop=FALSE]
    for(role in c("bank","outer")) {
      point <- sf::st_sfc(sf::st_point(xy[if(role=="bank")s[[paste0(side,"_bank_vertex")]] else
        if(side=="left")1L else nrow(xy),]),crs=crs)
      eligible <- which(chains$region_id==s[[paste0(side,"_region_id")]] & chains$role==role & chains$oriented)
      chain <- if(length(eligible))eligible[sf::st_nearest_feature(point,chains[eligible,])] else NA_integer_
      measure <- NA_real_; status <- "unavailable_chain"
      if(!is.na(chain)) {
        line <- sf::st_geometry(chains[chain,])
        measure <- as.numeric(sf::st_line_project(line,point))
        on_boundary <- as.numeric(sf::st_distance(point,line))<=tolerance
        if(role=="outer" && assign)point <- sf::st_line_sample(line,sample=measure/as.numeric(sf::st_length(line))) |>
          sf::st_cast("POINT")
        status <- if(!on_boundary && (role=="bank" || !assign))
          if(role=="outer" && !isTRUE(s[[paste0(side,"_supported")]]))"not_constructed" else "off_boundary" else "ordered"
      }
      rows[[length(rows)+1L]] <- sf::st_sf(candidate_id=s$candidate_id,Seq=s$Seq,side=side,role=role,
        region_id=s[[paste0(side,"_region_id")]],chain_id=if(is.na(chain))NA_character_ else chains$chain_id[chain],
        proposed_measure=measure,measure=measure,status=status,geometry=point)
    }
  }
  anchors <- if(length(rows))do.call(rbind,rows) else sf::st_sf(candidate_id=character(),Seq=integer(),
    side=character(),role=character(),region_id=character(),chain_id=character(),
    proposed_measure=double(),measure=double(),status=character(),geometry=sf::st_sfc(crs=crs))
  for(key in unique(stats::na.omit(anchors$chain_id))) {
    group <- which(anchors$chain_id==key & anchors$status=="ordered")
    group <- group[order(anchors$Seq[group])]
    if(!length(group))next
    if(anchors$role[group[1]]=="bank" || !assign) {
      reversed <- which(diff(anchors$measure[group])<=tolerance)
      if(length(reversed))anchors$status[group[unique(c(reversed,reversed+1L))]] <-
        paste0(anchors$role[group[1]],"_order_conflict")
      next
    }
    # Preserve the proposed outer positions as a set, assigning them in Seq
    # order. Strict separation prevents equal pooled endpoints from touching.
    line <- sf::st_geometry(chains[match(key,chains$chain_id),])
    length <- as.numeric(sf::st_length(line));gap <- tolerance*8
    positions <- sort(anchors$measure[group])
    if(length(group)>1L) {
      positions <- cummax(positions-gap*seq_along(group))+gap*seq_along(group)
      if(tail(positions,1)>length)positions <- positions-(tail(positions,1)-length)
    }
    if(any(positions<0) || any(positions>length)) {anchors$status[group]<-"insufficient_boundary";next}
    anchors$measure[group] <- positions
    sf::st_geometry(anchors)[group] <- sf::st_cast(sf::st_line_sample(line,sample=positions/length),"POINT")
  }
  anchors$measure_units <- rep(sf::st_crs(sections)$units_gdal,nrow(anchors))
  anchors
}

cross_section_review_order <- function(sections, chains, tolerance) {
  anchors <- cross_section_order_anchors(sections,chains,tolerance,assign=FALSE)
  for(side in c("left","right")) {
    sections[[paste0(side,"_order_status")]] <- vapply(seq_len(nrow(sections)),function(i) {
      if(sections[[paste0(side,"_leg_status")]][i]=="no_floodplain")return("no_floodplain")
      a <- anchors[anchors$candidate_id==sections$candidate_id[i] & anchors$side==side,]
      if(nrow(a)!=2L)return("unavailable_chain")
      bad <- a$status[a$status!="ordered"]
      if(length(bad))bad[1] else "ordered"
    },character(1))
    sections[[paste0(side,"_order_conflict")]] <- grepl("_order_conflict$",sections[[paste0(side,"_order_status")]])
  }
  channel <- cross_section_channel_anchors(sections,chains,tolerance)
  sections$channel_order_conflict <- rep(FALSE,nrow(sections))
  sections$channel_order_review <- rep(FALSE,nrow(sections))
  for(side in c("left","right")) {
    rows <- if(is.null(channel))NULL else channel[channel$side==side,]
    status <- if(is.null(rows))character() else rows$status[match(sections$candidate_id,rows$candidate_id)]
    sections[[paste0("channel_",side,"_order_status")]] <- status
    sections$channel_order_conflict <- sections$channel_order_conflict | status=="bank_order_conflict"
    sections$channel_order_review <- sections$channel_order_review | !status %in% c("ordered","single_section")
  }
  sections
}

# Repair an infeasible sorted target inside its neighbors' ordered interval
# before targets are frozen. Candidate positions use the original proposal and
# interval bounds, with GEOS visibility and the declared normal-angle bound.
cross_section_refine_targets <- function(sections,regions,chains,anchors,tolerance,angle_limit,
                                          margins=new.env(parent=emptyenv())) {
  crs <- sf::st_crs(sections)
  for(key in unique(stats::na.omit(anchors$chain_id[anchors$role=="outer"]))) {
    group <- which(anchors$chain_id==key & anchors$role=="outer" & anchors$status=="ordered")
    group <- group[order(anchors$Seq[group])]
    line <- sf::st_geometry(chains[match(key,chains$chain_id),]);len <- as.numeric(sf::st_length(line))
    for(k in seq_along(group)) {
      row <- group[k];i <- match(anchors$candidate_id[row],sections$candidate_id);side <- anchors$side[row]
      normal <- c(sections[[paste0(side,"_normal_dx")]][i],sections[[paste0(side,"_normal_dy")]][i])
      if(any(!is.finite(normal)))next
      xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
      bank <- xy[sections[[paste0(side,"_bank_vertex")]][i],]
      low <- if(k>1L)anchors$measure[group[k-1L]]+8*tolerance else 0
      high <- if(k<length(group))anchors$measure[group[k+1L]]-8*tolerance else len
      positions <- unique(c(anchors$measure[row],max(low,min(high,anchors$proposed_measure[row])),low,high,mean(c(low,high))))
      targets <- sf::st_cast(sf::st_line_sample(line,sample=positions/len),"POINT")
      coords <- sf::st_coordinates(targets)[,1:2,drop=FALSE]
      delta <- sweep(coords,2,bank)
      angles <- atan2(normal[1]*delta[,2]-normal[2]*delta[,1],drop(delta%*%normal))*180/pi
      id <- anchors$region_id[row]
      if(!exists(id,envir=margins,inherits=FALSE))assign(id,sf::st_buffer(
        sf::st_geometry(regions[regions$region_id==id,]),tolerance/1000,nQuadSegs=1),envir=margins)
      lines <- sf::st_sfc(lapply(seq_len(nrow(coords)),function(j)sf::st_linestring(rbind(bank,coords[j,]))),crs=crs)
      valid <- abs(angles)<=angle_limit+1e-7 & rowSums(delta^2)>tolerance^2 &
        sf::st_covered_by(lines,get(id,envir=margins),sparse=FALSE)[,1]
      if(valid[1] || !any(valid))next
      candidates <- which(valid)
      best <- candidates[order(abs(angles[candidates]),abs(positions[candidates]-anchors$measure[row]),positions[candidates])[1]]
      anchors$measure[row] <- positions[best];sf::st_geometry(anchors)[[row]] <- targets[[best]]
    }
  }
  anchors
}

# Fix outer targets first; construct complete legs in station order. Previously
# accepted legs and every other channel cut are barriers. A route may not slide
# its endpoint along the boundary to get around a neighbor.
cross_section_ordered_routes <- function(sections, regions, anchors, tolerance, allow_bends, angle_limit=30,
                                         margins=new.env(parent=emptyenv())) {
  crs <- sf::st_crs(sections)
  for(side in c("left","right"))sections[[paste0(side,"_anchor_status")]] <- rep("unavailable_chain",nrow(sections))
  channel <- sf::st_sfc(lapply(seq_len(nrow(sections)),function(i) {
    xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
    sf::st_linestring(xy[sections$left_bank_vertex[i]:sections$right_bank_vertex[i],,drop=FALSE])
  }),crs=crs)
  legs <- vector("list",2L*nrow(sections));accepted <- sf::st_sfc(crs=crs);pending <- list()
  for(i in order(sections$Seq))for(side in c("left","right")) {
    group <- 2L*(i-1L)+match(side,c("left","right"))
    bank <- sf::st_coordinates(channel[i])[if(side=="left")1L else 2L,1:2]
    legs[[group]] <- rbind(bank,bank)
    sections[[paste0(side,"_anchor_status")]][i] <- "no_floodplain"
    if(sections[[paste0(side,"_leg_status")]][i]=="no_floodplain")next
    a <- anchors[anchors$candidate_id==sections$candidate_id[i] & anchors$side==side &
      anchors$role %in% c("bank","outer"),,drop=FALSE]
    status <- if(nrow(a)!=2L)"unavailable_chain" else if(any(a$status!="ordered"))a$status[a$status!="ordered"][1] else "ordered"
    normal <- c(sections[[paste0(side,"_normal_dx")]][i],sections[[paste0(side,"_normal_dy")]][i])
    if(status=="ordered" && (length(normal)!=2L || any(!is.finite(normal))))status <- "direction_unavailable"
    route <- NULL;rotation <- NA_real_
    if(status=="ordered") {
      target <- sf::st_coordinates(a[a$role=="outer",])[,1:2,drop=FALSE][1,]
      polygon <- sf::st_geometry(regions[regions$region_id==a$region_id[1],])
      id <- a$region_id[1]
      if(!exists(id,envir=margins,inherits=FALSE))assign(id,sf::st_buffer(polygon,tolerance/1000,nQuadSegs=1),envir=margins)
      obstacles <- c(channel[-i],accepted)
      direct <- sf::st_sfc(sf::st_linestring(rbind(bank,target)),crs=crs)
      vector <- target-bank
      angle <- atan2(normal[1]*vector[2]-normal[2]*vector[1],sum(normal*vector))*180/pi
      if(abs(angle)<=angle_limit+1e-7 &&
          sf::st_covered_by(direct,get(id,envir=margins),sparse=FALSE)[1,1] &&
          !length(sf::st_intersects(direct,obstacles)[[1]]))route <- direct
      if(!is.null(route) && nrow(sf::st_coordinates(route))==2L) {
        if(abs(angle)>angle_limit+1e-7)route <- NULL else rotation <- angle
      }
      if(is.null(route) && allow_bends)pending[[length(pending)+1L]] <- list(i=i,side=side,group=group,
        bank=bank,target=target,polygon=polygon,angle=angle)
      if(is.null(route))status <- "no_ordered_route"
    }
    sections[[paste0(side,"_anchor_status")]][i] <- status
    sections[[paste0(side,"_supported")]][i] <- !is.null(route)
    sections[[paste0(side,"_routed")]][i] <- !is.null(route) && nrow(sf::st_coordinates(route))>2L
    sections[[paste0(side,"_rotation_deg")]][i] <- rotation
    sections[[paste0(side,"_leg_status")]][i] <- if(is.null(route))
      if(status=="direction_unavailable")status else "no_ordered_route" else "complete"
    if(!is.null(route)) {
      legs[[group]] <- sf::st_coordinates(route)[,1:2,drop=FALSE]
      accepted <- c(accepted,route)
    }
  }
  # Reserve the feasible straight layout before filling gaps. An early detour
  # must not needlessly force later, already feasible legs to acquire bends.
  for(task in pending) {
    i <- task$i;side <- task$side
    route <- cross_section_route_leg(task$bank,task$target,task$polygon,
      sf::st_sfc(sf::st_point(task$target),crs=crs),c(channel[-i],accepted),tolerance,fixed_target=TRUE)
    if(is.null(route))next
    coords <- sf::st_coordinates(route)[,1:2,drop=FALSE]
    if(nrow(coords)==2L && abs(task$angle)>angle_limit+1e-7)next
    legs[[task$group]] <- coords;accepted <- c(accepted,route)
    sections[[paste0(side,"_supported")]][i] <- TRUE
    sections[[paste0(side,"_routed")]][i] <- nrow(coords)>2L
    sections[[paste0(side,"_rotation_deg")]][i] <- if(nrow(coords)>2L)NA_real_ else task$angle
    sections[[paste0(side,"_leg_status")]][i] <- "complete"
    sections[[paste0(side,"_anchor_status")]][i] <- "ordered"
  }
  for(i in seq_len(nrow(sections))) {
    left <- legs[[2L*i-1L]];right <- legs[[2L*i]]
    sf::st_geometry(sections)[[i]] <- sf::st_linestring(rbind(left[nrow(left):1,,drop=FALSE],right))
    sections$left_bank_vertex[i] <- as.integer(nrow(left));sections$right_bank_vertex[i] <- as.integer(nrow(left)+1L)
  }
  sections
}
