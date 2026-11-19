# Plan targets and routes together only in difficult, bounded bend families.
# Existing complete layouts are feasible incumbents, never discarded for a
# prettier but incomplete proposal. Measures remain on actual polygon edges.
cross_section_bend_quality <- function(routes) {
  sum(vapply(routes,function(g) {
    if(is.null(g))return(0)
    xy <- sf::st_coordinates(g)[,1:2,drop=FALSE];v <- diff(xy)
    v <- v[sqrt(rowSums(v^2))>1e-9,,drop=FALSE]
    if(nrow(v)<2L)return(0)
    headings <- atan2(v[,2],v[,1]);turns <- (diff(headings)+pi)%%(2*pi)-pi
    # Densifying a smooth curve is not itself a shape defect. Count substantive
    # corners, accumulated turning and backtracking rather than raw vertices.
    sum(abs(turns))/pi + .05*sum(abs(turns)>pi/12) + sum(abs(turns)>pi/2)
  },numeric(1)))
}

# A valid incumbent is valuable geometric evidence in a tight passage. Deform
# it continuously toward a new ordered target before rebuilding a mesh path.
cross_section_retarget_route <- function(route,target,polygon,obstacles,tolerance) {
  if(is.null(route))return(NULL)
  safe <- function(g)sf::st_is_simple(g) && sf::st_covered_by(g,polygon,sparse=FALSE)[1,1] &&
    (!length(obstacles) || !length(sf::st_intersects(g,obstacles)[[1]]))
  xy <- sf::st_coordinates(route)[,1:2,drop=FALSE]
  delta <- target-xy[nrow(xy),]
  if(sqrt(sum(delta^2))<=tolerance/1000)return(if(safe(route))route else NULL)
  distance <- c(0,cumsum(sqrt(rowSums(diff(xy)^2))))
  if(tail(distance,1)<=tolerance)return(NULL)
  fractions <- sort(unique(c(distance/tail(distance,1),seq(0,1,length.out=9L))))
  points <- sf::st_coordinates(sf::st_cast(sf::st_line_sample(route,sample=fractions),"POINT"))[,1:2,drop=FALSE]
  for(power in c(2,4,8,1)) {
    proposal <- points+outer(fractions^power,delta)
    proposal[1,] <- xy[1,];proposal[nrow(proposal),] <- target
    g <- sf::st_sfc(sf::st_linestring(proposal),crs=sf::st_crs(route))
    if(safe(g))return(g)
  }
  NULL
}

cross_section_bend_plan <- function(sections,regions,chains,anchors,tolerance,angle_limit,
                                    max_sections=32L) {
  crs <- sf::st_crs(sections);meshes <- new.env(parent=emptyenv())
  channel <- sf::st_sfc(lapply(seq_len(nrow(sections)),function(i) {
    xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
    sf::st_linestring(xy[sections$left_bank_vertex[i]:sections$right_bank_vertex[i],,drop=FALSE])
  }),crs=crs)
  route_for <- function(i,side) {
    if(!isTRUE(sections[[paste0(side,"_supported")]][i]))return(NULL)
    xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
    at <- sections[[paste0(side,"_bank_vertex")]][i]
    xy <- if(side=="left")xy[at:1,,drop=FALSE] else xy[at:nrow(xy),,drop=FALSE]
    sf::st_sfc(sf::st_linestring(xy),crs=crs)
  }
  better <- function(a,b) {
    k <- which(abs(a-b)>1e-6);length(k)>0L && a[k[1L]]<b[k[1L]]
  }
  windows <- accepted <- attempts <- 0L
  for(key in unique(stats::na.omit(anchors$chain_id[anchors$role=="outer"]))) {
    rows <- which(anchors$chain_id==key & anchors$role=="outer" & anchors$status=="ordered")
    rows <- rows[order(anchors$Seq[rows])]
    if(length(rows)<2L)next
    side <- anchors$side[rows[1]];id <- anchors$region_id[rows[1]]
    ids <- match(anchors$candidate_id[rows],sections$candidate_id)
    bank <- t(vapply(ids,function(i)sf::st_coordinates(channel[i])[if(side=="left")1L else 2L,1:2],numeric(2)))
    bank_steps <- sqrt(rowSums(diff(bank)^2))
    desired <- max(8*tolerance,stats::median(bank_steps)/4)
    bends <- vapply(ids,function(i) {
      route <- route_for(i,side)
      if(is.null(route))Inf else nrow(sf::st_coordinates(route))-2L
    },numeric(1))
    gaps <- diff(anchors$measure[rows])
    crowded <- which(gaps<desired/2)
    seeds <- sort(unique(c(crowded,crowded+1L,which(bends>=2L))))
    if(!length(seeds))next
    # Overlapping neighborhoods form a whole bend, including complete awkward
    # legs. Bound each family rather than performing a reach-wide route search.
    blocks <- list()
    for(seed in seeds) {
      lo <- max(1L,seed-2L);hi <- min(length(rows),seed+2L)
      last <- length(blocks)
      if(last && lo<=tail(blocks[[last]],1)+1L && hi-blocks[[last]][1]+1L<=max_sections)
        blocks[[last]] <- seq.int(blocks[[last]][1],hi)
      else blocks[[last+1L]] <- seq.int(lo,hi)
    }
    line <- sf::st_geometry(chains[match(key,chains$chain_id),])
    len <- as.numeric(sf::st_length(line))
    # Match the initial router's numerical margin for bank/ray overlay roundoff.
    # Final review still measures containment against the original footprint.
    component <- sf::st_geometry(regions[regions$region_id==id,])
    polygon <- sf::st_buffer(component,tolerance/1000,nQuadSegs=1)
    for(block in blocks) {
      windows <- windows+1L;n <- length(block);ii <- ids[block];rr <- rows[block]
      lo <- min(block);hi <- max(block)
      low <- if(lo>1L)anchors$measure[rows[lo-1L]] else 0
      high <- if(hi<length(rows))anchors$measure[rows[hi+1L]] else len
      gap <- min(desired,(high-low)/(n+1L)*.7)
      if(!is.finite(gap) || gap<=8*tolerance)next
      old <- anchors$measure[rr]
      # Least-squares spreading with an adaptive gap, relaxed shifts and an
      # even allocation. Isotonic regression is a standard bounded-size tool;
      # GEOS routing, not the regression, determines spatial feasibility.
      spaced <- stats::isoreg(seq_len(n),old-gap*seq_len(n))$yf
      spaced <- pmax(low,pmin(high-gap*(n+1L),spaced))+gap*seq_len(n)
      uniform <- low+(high-low)*seq_len(n)/(n+1L)
      allocations <- list(spaced,(old+spaced)/2,(3*old+spaced)/4,uniform,old)
      original <- lapply(ii,route_for,side=side)
      spacing_cost <- function(m) {
        d <- diff(c(low,m,high));mean(pmax(0,1-d/gap)^2)
      }
      score <- function(routes,m) c(-sum(!vapply(routes,is.null,logical(1))),
        spacing_cost(m),cross_section_bend_quality(routes))
      best <- original;best_m <- old;best_score <- score(original,old)
      # External sections (including the other bank's leg) remain hard barriers.
      external <- sf::st_geometry(sections[-ii,])
      opposite <- Filter(Negate(is.null),lapply(ii,route_for,side=if(side=="left")"right" else "left"))
      if(length(opposite))external <- c(external,do.call(c,opposite))
      orders <- list(seq_len(n),rev(seq_len(n)),order(abs(seq_len(n)-(n+1)/2)))
      for(m in allocations) {
        if(any(diff(c(low,m,high))<=tolerance))next
        # Never spend routing time on a distribution already worse than a
        # complete incumbent. Same-distribution trials can improve shape.
        if(-best_score[1]==n && spacing_cost(m)>best_score[2]+1e-6)next
        targets <- sf::st_coordinates(sf::st_cast(sf::st_line_sample(line,sample=m/len),"POINT"))[,1:2,drop=FALSE]
        for(priority in orders) {
          proposed <- vector("list",n);reservations <- external
          for(j in priority) {
            i <- ii[j];a <- bank[block[j],];b <- targets[j,]
            normal <- c(sections[[paste0(side,"_normal_dx")]][i],sections[[paste0(side,"_normal_dy")]][i])
            if(any(!is.finite(normal)))next
            obstacles <- c(channel[-i],reservations)
            vector <- b-a;angle <- atan2(normal[1]*vector[2]-normal[2]*vector[1],sum(normal*vector))*180/pi
            route <- sf::st_sfc(sf::st_linestring(rbind(a,b)),crs=crs)
            safe <- function(g)sf::st_is_simple(g) && sf::st_covered_by(g,polygon,sparse=FALSE)[1,1] &&
              (!length(obstacles) || !length(sf::st_intersects(g,obstacles)[[1]]))
            if(abs(angle)>angle_limit+1e-7 || !safe(route))route <- NULL
            if(is.null(route))route <- cross_section_angle_bend(a,b,normal,polygon,obstacles,angle_limit)
            if(is.null(route))route <- cross_section_retarget_route(original[[j]],b,polygon,obstacles,tolerance)
            if(is.null(route)) {
              if(!exists(id,envir=meshes,inherits=FALSE))assign(id,cross_section_mesh(component),envir=meshes)
              mesh <- get(id,envir=meshes)
              if(!is.null(mesh))route <- cross_section_mesh_path(a,b,polygon,mesh,obstacles,tolerance,desired)
            }
            if(is.null(route))next
            if(nrow(sf::st_coordinates(route))==2L && abs(angle)>angle_limit+1e-7)next
            proposed[[j]] <- route;reservations <- c(reservations,route)
          }
          attempts <- attempts+1L;candidate_score <- score(proposed,m)
          if(better(candidate_score,best_score)) {best<-proposed;best_m<-m;best_score<-candidate_score}
          if(-candidate_score[1]==n && candidate_score[2]<1e-8 && candidate_score[3]<1e-8)break
        }
      }
      # Do not sacrifice any already complete member to complete a different
      # one, even when the family count is tied.
      if(any(vapply(best,is.null,logical(1))) || !better(best_score,score(original,old)) ||
          any(!vapply(original,is.null,logical(1)) & vapply(best,is.null,logical(1))))next
      accepted <- accepted+1L
      for(j in seq_len(n)) {
        if(is.null(best[[j]]))next
        i <- ii[j];xy <- sf::st_coordinates(sections[i,])[,1:2,drop=FALSE]
        route <- sf::st_coordinates(best[[j]])[,1:2,drop=FALSE]
        if(side=="left") {
          xy <- rbind(route[nrow(route):1,,drop=FALSE],xy[sections$right_bank_vertex[i]:nrow(xy),,drop=FALSE])
          sections$left_bank_vertex[i] <- as.integer(nrow(route))
          sections$right_bank_vertex[i] <- as.integer(nrow(route)+1L)
        } else xy <- rbind(xy[seq_len(sections$left_bank_vertex[i]),,drop=FALSE],route)
        sf::st_geometry(sections)[[i]] <- sf::st_linestring(xy)
        sections[[paste0(side,"_supported")]][i] <- TRUE
        sections[[paste0(side,"_routed")]][i] <- nrow(route)>2L
        sections[[paste0(side,"_group_routed")]][i] <- TRUE
        sections[[paste0(side,"_anchor_status")]][i] <- "ordered"
        sections[[paste0(side,"_leg_status")]][i] <- "complete"
        v <- route[2,]-route[1,]
        normal <- c(sections[[paste0(side,"_normal_dx")]][i],sections[[paste0(side,"_normal_dy")]][i])
        sections[[paste0(side,"_rotation_deg")]][i] <- if(nrow(route)>2L)NA_real_ else
          atan2(normal[1]*v[2]-normal[2]*v[1],sum(normal*v))*180/pi
        anchors$measure[rr[j]] <- best_m[j]
        sf::st_geometry(anchors)[[rr[j]]] <- sf::st_point(tail(route,1)[1,])
      }
    }
  }
  list(sections=sections,anchors=anchors,windows=windows,accepted=accepted,attempts=attempts)
}
