# Joint angular deconfliction of channel runs. No width/length objective:
# search increasing angular envelopes, retaining full connected channel cuts.
cross_section_channel_deconflict <- function(sections, channel, eligible, options_for,
                                            tolerance, angle_limit, max_nodes=50000L) {
  n <- nrow(sections);crs <- sf::st_crs(sections)
  result <- list(sections=sections,angle=rep(0,n),status=rep("perpendicular",n),nodes=0L)
  if(n<2L)return(result)
  hits <- sf::st_intersects(sections);contact <- lengths(hits)>1L
  result$status[contact] <- "unresolved_contact"
  if(!any(eligible) || angle_limit==0)return(result)
  rings <- suppressWarnings(sf::st_cast(sf::st_boundary(sf::st_cast(
    sf::st_union(sf::st_geometry(channel)),"POLYGON")),"LINESTRING"))
  length <- as.numeric(sf::st_length(rings))
  wrap <- function(x,len)(x+len/2)%%len-len/2
  points <- lapply(1:2,function(side)sf::st_sfc(lapply(sf::st_geometry(sections),
    function(z)sf::st_point(z[side,])),crs=crs))
  parts <- lapply(points,sf::st_nearest_feature,y=rings)
  raw <- base <- lapply(1:2,function(side)rep(NA_real_,n))
  directions <- lapply(1:2,function(side)rep(NA_real_,length(rings)))
  for(side in 1:2)for(part in unique(parts[[side]])) {
    ids <- which(parts[[side]]==part);ids <- ids[order(sections$Seq[ids])]
    m <- as.numeric(sf::st_line_project(rings[part],points[[side]][ids]))
    delta <- wrap(diff(m),length[part]);direction <- sign(stats::median(delta))
    if(!is.finite(direction) || direction==0)next
    raw[[side]][ids] <- m;base[[side]][ids] <- c(0,cumsum(delta))*direction
    directions[[side]][part] <- direction
    reversed <- which(diff(base[[side]][ids])<=tolerance)
    if(length(reversed)) {
      bad <- ids[unique(c(reversed,reversed+1L))]
      result$status[bad[!contact[bad]]] <- "unresolved_order"
      contact[bad] <- TRUE
    }
  }
  if(!any(contact & eligible))return(result)
  # Include two clear neighbors on either side of a contact, where eligible.
  # Bound native matrix allocation on very long reaches; exhausted windows
  # remain explicit instead of publishing a partial or intersecting repair.
  active <- which(eligible & vapply(seq_len(n),function(i)any(abs(i-which(contact & eligible))<=2L),logical(1)))
  breaks <- c(TRUE,diff(sections$Seq[active])!=1L |
    diff(parts[[1]][active])!=0L | diff(parts[[2]][active])!=0L)
  runs <- split(active,cumsum(breaks))
  for(run in runs) {
    if(!any(contact[run]))next
    if(length(run)>64L) {result$status[run] <- "search_limit";next}
    if(any(!is.finite(unlist(lapply(base,`[`,run))))) {
      result$status[run] <- "unavailable_bank_order";next
    }
    fixed <- setdiff(seq_len(n),run);rows <- list();geometry <- list()
    for(i in run)for(option in options_for(i)) {
      rows[[length(rows)+1L]] <- data.frame(section=i,angle=option$angle)
      geometry[[length(geometry)+1L]] <- sf::st_linestring(unname(option$banks))
    }
    if(!length(rows))next
    meta <- do.call(rbind,rows);geometry <- sf::st_sfc(geometry,crs=crs)
    clear <- lengths(sf::st_intersects(geometry,sf::st_geometry(result$sections)[fixed]))==0L &
      abs(meta$angle)<=angle_limit+1e-7
    measure <- vector("list",2L)
    for(side in 1:2) {
      p <- sf::st_sfc(lapply(geometry,function(z)sf::st_point(z[side,])),crs=crs)
      part <- sf::st_nearest_feature(p,rings);expected <- parts[[side]][meta$section]
      clear <- clear & part==expected
      measure[[side]] <- rep(NA_real_,nrow(meta))
      for(k in unique(expected)) {
        ids <- which(expected==k);station <- meta$section[ids]
        m <- as.numeric(sf::st_line_project(rings[k],p[ids]))
        measure[[side]][ids] <- base[[side]][station]+wrap(m-raw[[side]][station],length[k])*directions[[side]][k]
      }
      for(j in seq_len(nrow(meta))) {
        before <- fixed[parts[[side]][fixed]==expected[j] & sections$Seq[fixed]<sections$Seq[meta$section[j]]]
        after <- fixed[parts[[side]][fixed]==expected[j] & sections$Seq[fixed]>sections$Seq[meta$section[j]]]
        clear[j] <- clear[j] && is.finite(measure[[side]][j]) &&
          all(base[[side]][before]<measure[[side]][j]-tolerance) &&
          all(base[[side]][after]>measure[[side]][j]+tolerance)
      }
    }
    pairs <- sf::st_intersects(geometry,sparse=FALSE)
    compatible <- !pairs
    rank <- outer(sections$Seq[meta$section],sections$Seq[meta$section],"-")
    for(side in 1:2) {
      same <- outer(parts[[side]][meta$section],parts[[side]][meta$section],"==")
      ordered <- outer(measure[[side]],measure[[side]],"-")*sign(rank)>tolerance
      compatible <- compatible & (!same | ordered)
    }
    compatible[is.na(compatible)] <- FALSE
    selected <- NULL;exhausted <- FALSE;nodes <- 0L
    search <- function(domains,chosen=rep(NA_integer_,length(run))) {
      nodes <<- nodes+1L
      if(nodes>max_nodes) {exhausted <<- TRUE;return(NULL)}
      if(any(!lengths(domains)))return(NULL)
      if(all(!is.na(chosen)))return(chosen)
      remaining <- which(is.na(chosen));g <- remaining[which.min(lengths(domains)[remaining])]
      for(j in domains[[g]]) {
        next_domains <- domains;next_chosen <- chosen;next_chosen[g] <- j
        for(h in setdiff(remaining,g))next_domains[[h]] <- domains[[h]][compatible[j,domains[[h]]]]
        answer <- search(next_domains,next_chosen)
        if(!is.null(answer) || exhausted)return(answer)
      }
      NULL
    }
    # First feasible envelope minimizes the largest tested departure, subject
    # to the finite grid and explicit search budget. No infeasible partial repair.
    for(bound in sort(unique(abs(meta$angle[abs(meta$angle)<=angle_limit+1e-7])))) {
      domains <- lapply(run,function(i) {
        ids <- which(meta$section==i & clear & abs(meta$angle)<=bound+1e-7)
        ids[order(abs(meta$angle[ids]),meta$angle[ids])]
      })
      if(any(!lengths(domains)))next
      selected <- search(domains)
      if(!is.null(selected) || exhausted)break
    }
    result$nodes <- result$nodes+nodes
    if(is.null(selected)) {result$status[run] <- if(exhausted)"search_limit" else "no_feasible_grid";next}
    # Within the feasible envelope, prefer smaller individual departures while
    # preserving every pair constraint. This is not a global sum-optimal claim.
    repeat {
      changed <- FALSE
      for(g in seq_along(run)) {
        ids <- domains[[g]][abs(meta$angle[domains[[g]]])<abs(meta$angle[selected[g]])-1e-7]
        for(j in ids)if(all(compatible[j,selected[-g]])) {
          selected[g] <- j;changed <- TRUE;break
        }
      }
      if(!changed)break
    }
    trial <- result$sections;sf::st_geometry(trial)[run] <- geometry[selected]
    # Audit actual selected geometry and actual directed bank arcs, independently
    # of the cyclic ring coordinates used to prune the finite search.
    chains <- cross_section_channel_bank_chains(trial,channel,tolerance)
    anchors <- cross_section_channel_anchors(trial,chains,tolerance)
    actual <- anchors$status[anchors$candidate_id %in% trial$candidate_id[run]]
    safe <- all(lengths(sf::st_intersects(trial))[run]==1L) &&
      length(actual)==2L*length(run) && all(actual %in% c("ordered","single_section"))
    if(!safe) {result$status[run] <- "unresolved_order";next}
    result$sections <- trial;result$angle[run] <- meta$angle[selected]
    result$status[run] <- ifelse(abs(result$angle[run])>1e-7,"resolved_rotation","perpendicular")
  }
  result
}
