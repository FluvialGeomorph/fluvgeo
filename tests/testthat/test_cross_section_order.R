test_that("outer anchors preserve station rank and fixed targets produce disjoint legs", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line,f$channel,f$floodplain)
  s <- x$sections[1:3,]
  original <- lapply(sf::st_geometry(s),function(g)sf::st_coordinates(g)[2:3,1:2,drop=FALSE])
  for(i in 1:3) {
    xy <- sf::st_coordinates(s[i,])[,1:2,drop=FALSE]
    xy[1,1] <- c(50,10,30)[i]
    sf::st_geometry(s)[[i]] <- sf::st_linestring(xy)
  }
  before <- cross_section_order_anchors(s,x$boundary_chains,.001,assign=FALSE)
  expect_true(any(before$status=="outer_order_conflict"))
  anchors <- cross_section_order_anchors(s,x$boundary_chains,.001)
  left <- anchors[anchors$side=="left" & anchors$role=="outer",]
  expect_true(all(diff(left$measure)>0))
  expect_equal(sort(left$measure),sort(left$proposed_measure))
  result <- cross_section_ordered_routes(s,x$regions,anchors,.001,FALSE,60)
  result <- cross_section_layout_review(result,f$floodplain,x$boundary_chains)
  expect_false(any(result$conflict))
  expect_false(any(result$left_order_conflict | result$right_order_conflict))
  expect_true(all(result$left_supported & result$right_supported))
  for(i in 1:3) {
    xy <- sf::st_coordinates(result[i,])[,1:2,drop=FALSE]
    expect_equal(unname(xy[result$left_bank_vertex[i]:result$right_bank_vertex[i],]),unname(original[[i]]))
    expect_equal(unname(xy[1,]),unname(sf::st_coordinates(left[i,])[1,1:2]))
  }
  reviewed <- cross_section_layout_review(s,f$floodplain,x$boundary_chains)
  expect_true(any(reviewed$left_order_conflict))
  expect_equal(sf::st_coordinates(reviewed),sf::st_coordinates(s))
  routed <- cross_section_layout(f$line,f$channel,f$floodplain,allow_overbank_bends=TRUE)
  reused <- cross_section_ordered_routes(x$sections,x$regions,x$boundary_anchors,.001,TRUE)
  expect_equal(sf::st_coordinates(reused),sf::st_coordinates(routed$sections))
})

test_that("fixed targets cannot slide to another boundary to evade an obstacle", {
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(10,0),c(10,10),c(0,10),c(0,0)))),crs=26915)
  barrier <- sf::st_sfc(sf::st_linestring(rbind(c(8,0),c(2,10))),crs=26915)
  expect_null(cross_section_route_leg(c(2,0),c(8,10),polygon,sf::st_boundary(polygon),barrier,.001,fixed_target=TRUE))
})

test_that("filling an early gap preserves a feasible later straight leg", {
  rectangle <- function(x0,y0,x1,y1)sf::st_polygon(list(rbind(c(x0,y0),c(x1,y0),c(x1,y1),c(x0,y1),c(x0,y0))))
  polygon <- sf::st_difference(sf::st_sfc(rectangle(0,0,10,10),crs=26915),
    sf::st_sfc(rectangle(3,0,7,7),crs=26915))
  regions <- sf::st_sf(region_id="p",geometry=polygon)
  sections <- sf::st_sf(candidate_id=c("a","b"),Seq=1:2,left_bank_vertex=2L,right_bank_vertex=3L,
    left_region_id="p",right_region_id=NA_character_,left_leg_status="complete",right_leg_status="no_floodplain",
    left_supported=TRUE,right_supported=FALSE,left_normal_dx=1,left_normal_dy=0,
    geometry=sf::st_sfc(lapply(c(2,8),function(y)sf::st_linestring(rbind(c(10,y),c(0,y),c(-1,y),c(-1,y)))),crs=26915))
  anchors <- sf::st_sf(candidate_id=rep(c("a","b"),each=2),Seq=rep(1:2,each=2),side="left",
    role=rep(c("bank","outer"),2),region_id="p",status="ordered",
    geometry=sf::st_sfc(sf::st_point(c(0,2)),sf::st_point(c(10,2)),sf::st_point(c(0,8)),sf::st_point(c(10,8)),crs=26915))
  straight <- cross_section_ordered_routes(sections,regions,anchors,.001,FALSE)
  routed <- cross_section_ordered_routes(sections,regions,anchors,.001,TRUE)
  expect_false(straight$left_supported[1]);expect_true(all(routed$left_supported))
  expect_identical(sf::st_as_binary(sf::st_geometry(straight[2,])),sf::st_as_binary(sf::st_geometry(routed[2,])))
  expect_equal(nrow(sf::st_coordinates(routed[2,])),3L)
  expect_length(sf::st_intersects(routed[1,],routed[2,])[[1]],0)
})

test_that("whole-channel bank rank is checked independently of floodplain routing", {
  f <- layout_fixture();x <- cross_section_layout(f$line,f$channel,f$floodplain)
  s <- x$sections
  for(i in 2:3) {
    xy <- sf::st_coordinates(s[i,])[,1:2,drop=FALSE]
    xy[s$right_bank_vertex[i],1] <- c(50,30)[i-1L]
    sf::st_geometry(s)[[i]] <- sf::st_linestring(xy)
  }
  before <- sf::st_as_binary(sf::st_geometry(s))
  reviewed <- cross_section_layout_review(s,f$floodplain,x$boundary_chains)
  expect_true(all(reviewed$channel_order_conflict[2:3]))
  expect_false(any(reviewed$channel_order_conflict[-(2:3)]))
  expect_true(all(reviewed$channel_skewed[2:3]))
  expect_identical(sf::st_as_binary(sf::st_geometry(reviewed)),before)
})

test_that("a narrower confluence crossing cannot override channel perpendicularity", {
  f <- layout_fixture()
  xy <- rbind(c(0,-5),c(45,-5),c(55,-25),c(65,-5),c(200,-5),c(200,5),c(0,5),c(0,-5))
  f$channel <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(xy)),crs=26915))
  a <- cross_section_regions(f$line,f$channel,f$floodplain)
  sf::st_geometry(a$centerlines)[[which(a$centerlines$region=="channel")]] <- sf::st_geometry(f$line)[[1]]
  x <- cross_section_layout(f$line,f$channel,f$floodplain,region_artifacts=a)
  expect_equal(x$sections$channel_rotation_deg,rep(0,nrow(x$sections)))
  s <- x$sections[3,];z <- sf::st_coordinates(s)[s$left_bank_vertex:s$right_bank_vertex,1:2]
  expect_equal(unname(z[,1]),c(50,50))
  slant <- sf::st_sfc(sf::st_linestring(rbind(c(50,0)-100*c(.5,sqrt(3)/2),c(50,0)+100*c(.5,sqrt(3)/2))),crs=26915)
  narrower <- suppressWarnings(sf::st_intersection(slant,sf::st_geometry(f$channel)))
  expect_lt(sum(as.numeric(sf::st_length(narrower))),sqrt(sum((z[2,]-z[1,])^2)))
})
