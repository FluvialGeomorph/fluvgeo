test_that("channel-centerline lobe cleanup preserves the mainstem and original banks", {
  rectangle <- function(x0,y0,x1,y1)sf::st_polygon(list(rbind(c(x0,y0),c(x1,y0),c(x1,y1),c(x0,y1),c(x0,y0))))
  main <- sf::st_sfc(rectangle(0,-5,100,5),crs=26915)
  raw <- sf::st_sf(geometry=sf::st_union(main,sf::st_sfc(rectangle(48,4,52,40),crs=26915)))
  spine <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(rbind(c(0,0),c(100,0))),crs=26915))
  result <- cross_section_mainstem_channel(spine,raw,2)
  expect_equal(sum(result$branches$removed),1L)
  expect_true(sf::st_covered_by(main,result$channel,sparse=FALSE)[1,1])
  expect_true(sf::st_covered_by(spine,result$channel,sparse=FALSE)[1,1])
  expect_true(all(sf::st_is_valid(result$channel)))
  removed <- result$branches[result$branches$removed,]
  expect_equal(as.numeric(sf::st_area(raw)-sf::st_area(result$channel)),as.numeric(sf::st_area(removed)),tolerance=1e-7)
  expect_false(sf::st_intersects(spine,removed,sparse=FALSE)[1,1])
  expect_true(all(removed$measure_units==sf::st_crs(raw)$units_gdal))
  unchanged <- cross_section_mainstem_channel(spine,sf::st_sf(geometry=main),2)
  expect_equal(sf::st_coordinates(unchanged$channel),sf::st_coordinates(main))
  factor <- 3937/1200
  feet_raw <- sf::st_sf(geometry=sf::st_set_crs(sf::st_set_crs(sf::st_geometry(raw),NA)*factor,2277))
  feet_spine <- sf::st_sf(geometry=sf::st_set_crs(sf::st_set_crs(sf::st_geometry(spine),NA)*factor,2277))
  feet <- cross_section_mainstem_channel(feet_spine,feet_raw,2*factor)
  expect_equal(sum(feet$branches$removed),1L)
  expect_true(all(feet$branches$measure_units==sf::st_crs(feet_raw)$units_gdal))
  expect_equal(as.numeric(sf::st_area(feet$channel))/factor^2,
    as.numeric(sf::st_area(result$channel)),tolerance=1e-6)
})

test_that("channel cleanup precedes a complete nonoverlapping three-region partition", {
  f <- layout_fixture()
  arm <- sf::st_sfc(sf::st_polygon(list(rbind(c(98,4),c(102,4),c(102,25),c(98,25),c(98,4)))),crs=26915)
  raw <- sf::st_sf(geometry=sf::st_union(sf::st_geometry(f$channel),arm))
  result <- cross_section_regions(f$line,raw,f$floodplain)
  expect_true(any(result$channel_branches$removed))
  expect_lt(as.numeric(sf::st_area(result$channel_footprint)),as.numeric(sf::st_area(raw)))
  expect_identical(sf::st_as_binary(sf::st_geometry(result$source_channel)),sf::st_as_binary(sf::st_geometry(raw)))
  expect_equal(sum(as.numeric(sf::st_area(result$region_polygons))),as.numeric(sf::st_area(f$floodplain)),tolerance=1e-6)
  expect_true(all(sf::st_covered_by(result$channel_reference,result$channel_footprint,sparse=FALSE)))
})

test_that("channel cleanup removes hooked, stubby and narrow arms but retains a broad bank", {
  rectangle <- function(x0,y0,x1,y1)sf::st_polygon(list(rbind(c(x0,y0),c(x1,y0),c(x1,y1),c(x0,y1),c(x0,y0))))
  main <- sf::st_sfc(rectangle(0,-5,120,5),crs=26915)
  arms <- sf::st_sfc(rectangle(15,4,17,16),rectangle(16,14,27,16),
    rectangle(35,4,41,10),rectangle(50,4,51,13),rectangle(95,4,99,35),crs=26915)
  broad <- sf::st_sfc(rectangle(65,4,85,8),crs=26915)
  raw <- sf::st_sf(geometry=sf::st_union(c(main,arms,broad)))
  spine <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(rbind(c(0,0),c(120,0))),crs=26915))
  result <- cross_section_mainstem_channel(spine,raw,2,padding=.5,min_depth=1)
  tips <- sf::st_sfc(lapply(list(c(25,15),c(38,9),c(50.5,12),c(97,33)),sf::st_point),crs=26915)
  expect_false(any(sf::st_intersects(tips,result$channel,sparse=FALSE)))
  expect_true(sf::st_covered_by(main,result$channel,sparse=FALSE)[1,1])
  expect_true(sf::st_covered_by(broad,result$channel,sparse=FALSE)[1,1])
  expect_true(all(sf::st_is_valid(result$channel)))
  expect_equal(length(sf::st_cast(sf::st_geometry(result$channel),'POLYGON')),1L)
})

test_that("channel cleanup preserves a winding constant-width mainstem", {
  spine <- sf::st_sfc(sf::st_linestring(rbind(c(0,0),c(30,0),c(35,10),c(30,20),c(0,20))),crs=26915)
  raw <- sf::st_sf(geometry=sf::st_buffer(spine,4,nQuadSegs=8))
  result <- cross_section_mainstem_channel(sf::st_sf(geometry=spine),raw,2,padding=.5,min_depth=1)
  expect_identical(sf::st_as_binary(sf::st_geometry(result$channel)),sf::st_as_binary(sf::st_geometry(raw)))
  expect_false(any(result$branches$removed))
})

test_that("a tiny disconnected skeleton fragment cannot steal a main-route anchor", {
  skeleton <- sf::st_sfc(sf::st_multilinestring(list(rbind(c(0,5),c(1,5)),rbind(c(2,0),c(100,0)))),crs=26915)
  flowline <- sf::st_sfc(sf::st_linestring(rbind(c(0,0),c(100,0))),crs=26915)
  polygon <- sf::st_buffer(flowline,10)
  before <- sf::st_as_binary(skeleton)
  result <- cross_section_coherent_route(skeleton,flowline,polygon)
  expect_equal(as.numeric(sf::st_length(result$route)),98)
  expect_equal(result$span_fraction,.98)
  expect_true(sf::st_covered_by(result$route,polygon,sparse=FALSE)[1,1])
  expect_identical(sf::st_as_binary(skeleton),before)
})

test_that("an ordered target beyond the straight angle bound can use a genuine bounded bend", {
  polygon<-sf::st_sfc(sf::st_polygon(list(rbind(c(0,-10),c(20,-10),c(20,20),c(0,20),c(0,-10)))),crs=26915)
  route<-cross_section_angle_bend(c(0,0),c(10,10),c(1,0),polygon,sf::st_sfc(crs=26915),30)
  xy<-sf::st_coordinates(route)[,1:2,drop=FALSE]
  expect_equal(nrow(xy),3L)
  expect_equal(unname(xy[1,]),c(0,0));expect_equal(unname(xy[3,]),c(10,10))
  expect_equal(atan2(xy[2,2],xy[2,1])*180/pi,30)
  expect_true(sf::st_covered_by(route,polygon,sparse=FALSE)[1,1])
  expect_null(cross_section_angle_bend(c(0,0),c(10,1),c(1,0),polygon,sf::st_sfc(crs=26915),30))
  obstacle<-sf::st_sfc(sf::st_linestring(rbind(c(3,-5),c(3,15))),crs=26915)
  expect_null(cross_section_angle_bend(c(0,0),c(10,10),c(1,0),polygon,obstacle,30))
})

test_that("a turning endpoint chord does not relabel channel-bank direction", {
  f <- layout_fixture();s <- cross_section_layout(f$line,f$channel,f$floodplain)$sections[3,]
  xy <- sf::st_coordinates(s)[,1:2,drop=FALSE]
  sf::st_geometry(s)[[1]] <- sf::st_linestring(rbind(xy[1:3,],c(80,30),c(100,-40),c(80,-40)))
  reviewed <- cross_section_layout_review(s,f$floodplain)
  expect_true(reviewed$channel_oriented)
  expect_false(reviewed$outer_chord_oriented)
  expect_false(reviewed$oriented)
})

test_that("geodesic direction association reaches a hidden polygon centerline", {
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(10,0),c(10,2),c(2,2),c(2,10),c(0,10),c(0,0)))))
  path <- sf::st_sfc(sf::st_linestring(rbind(c(1,4),c(1,9))))
  bank <- c(9,0)
  direct <- sf::st_nearest_points(sf::st_sfc(sf::st_point(bank)),path)
  expect_false(sf::st_covered_by(direct,polygon,sparse=FALSE)[1,1])
  mesh <- cross_section_mesh(polygon)
  result <- cross_section_geodesic_match(bank,path,polygon,mesh,.001)
  expect_false(is.null(result))
  expect_true(sf::st_covered_by(result$route,polygon,sparse=FALSE)[1,1])
  expect_gte(nrow(sf::st_coordinates(result$route)),3L)
  expect_equal(as.numeric(sf::st_distance(sf::st_sfc(sf::st_point(result$point)),path)),0)
  expect_equal(unname(sf::st_coordinates(result$route)[1,1:2]),bank)
  expect_equal(result,cross_section_geodesic_match(bank,path,polygon,mesh,.001))
  spaced <- cross_section_mesh_path(bank,result$point,polygon,mesh,clearance=2)
  expect_false(is.null(spaced))
  expect_true(sf::st_covered_by(spaced,polygon,sparse=FALSE)[1,1])
  expect_equal(unname(sf::st_coordinates(spaced)[1,1:2]),bank)
  barrier <- sf::st_sfc(sf::st_linestring(rbind(c(0,3),c(2,3))))
  expect_null(cross_section_mesh_path(bank,c(1,9),polygon,mesh,barrier,.001))
})

test_that("group search preserves a feasible L-shaped family and its fixed endpoints", {
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(10,0),c(10,10),c(8,10),c(8,2),c(0,2),c(0,0)))),crs=26915)
  banks <- cbind(0,c(.5,1,1.5));targets <- cbind(c(9.5,9,8.5),10)
  sections <- sf::st_sf(candidate_id=letters[1:3],Seq=1:3,left_bank_vertex=2L,right_bank_vertex=3L,
    left_region_id='p',right_region_id=NA_character_,left_leg_status='complete',right_leg_status='no_floodplain',
    left_supported=TRUE,right_supported=FALSE,left_normal_dx=1,left_normal_dy=0,left_geodesic_direction=TRUE,
    geometry=sf::st_sfc(lapply(1:3,function(i)sf::st_linestring(rbind(targets[i,],banks[i,],c(-1,banks[i,2])))),crs=26915))
  anchors <- sf::st_sf(candidate_id=rep(letters[1:3],each=2),Seq=rep(1:3,each=2),side='left',
    role=rep(c('bank','outer'),3),region_id='p',status='ordered',
    geometry=sf::st_sfc(unlist(lapply(1:3,function(i)list(sf::st_point(banks[i,]),sf::st_point(targets[i,]))),recursive=FALSE),crs=26915))
  result <- cross_section_ordered_routes(sections,sf::st_sf(region_id='p',geometry=polygon),anchors,.001,TRUE,60)
  initial <- sections;initial$left_geodesic_direction <- FALSE
  initial <- cross_section_ordered_routes(initial,sf::st_sf(region_id='p',geometry=polygon),anchors,.001,TRUE,60)
  left_routes <- function(s)lapply(seq_len(nrow(s)),function(i) {
    xy<-sf::st_coordinates(s[i,])[,1:2,drop=FALSE]
    sf::st_sfc(sf::st_linestring(xy[seq_len(s$left_bank_vertex[i]),]),crs=26915)
  })
  expect_true(all(result$left_supported))
  expect_lte(cross_section_route_crowding(left_routes(result),.125),
    cross_section_route_crowding(left_routes(initial),.125)+1e-8)
  expect_true(all(lengths(sf::st_intersects(result))==1L))
  for(i in 1:3) {
    xy <- sf::st_coordinates(result[i,])[,1:2,drop=FALSE]
    expect_equal(unname(xy[1,]),unname(targets[i,]))
    expect_equal(unname(xy[result$left_bank_vertex[i],]),unname(banks[i,]))
    leg <- sf::st_sfc(sf::st_linestring(xy[seq_len(result$left_bank_vertex[i]),]),crs=26915)
    expect_true(sf::st_covered_by(leg,polygon,sparse=FALSE)[1,1])
  }
})

test_that("interior crowding distinguishes compressed from separated route families", {
  line <- function(y)sf::st_sfc(sf::st_linestring(rbind(c(0,y),c(10,y))),crs=26915)
  expect_gt(cross_section_route_crowding(list(line(0),line(.01)),1),.9)
  expect_equal(cross_section_route_crowding(list(line(0),line(2)),1),0)
})
