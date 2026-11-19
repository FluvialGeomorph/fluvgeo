channel_search_fixture <- function() {
  polygon <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(0,0),c(100,0),c(100,100),c(0,100),c(0,0)))),crs=26915))
  banks <- list(rbind(c(10,0),c(10,100)),rbind(c(30,0),c(70,100)),
    rbind(c(50,0),c(30,100)),rbind(c(90,0),c(90,100)))
  sections <- sf::st_sf(candidate_id=letters[1:4],Seq=1:4,left_bank_vertex=1L,right_bank_vertex=2L,
    geometry=sf::st_sfc(lapply(banks,sf::st_linestring),crs=26915))
  options <- function(i) {
    base <- list(list(banks=banks[[i]],angle=0))
    if(i %in% 2:3)base <- c(base,list(list(banks=rbind(c(c(30,50)[i-1L],0),c(c(30,50)[i-1L],100)),
      angle=c(atan(.4),-atan(.2))[i-1L]*180/pi)))
    base
  }
  list(sections=sections,channel=polygon,options=options)
}

test_that("joint channel search enforces bank rank, full width and all-pair separation", {
  f <- channel_search_fixture()
  z <- cross_section_channel_deconflict(f$sections,f$channel,rep(TRUE,4),f$options,.001,30)
  expect_true(all(lengths(sf::st_intersects(z$sections))==1L))
  for(i in 1:4) {
    xy <- sf::st_coordinates(z$sections[i,])[,1:2]
    expect_equal(unname(xy[,2]),c(0,100))
    expect_equal(unname(xy[,1]),rep(c(10,30,50,90)[i],2))
  }
  expect_identical(z$sections$candidate_id,f$sections$candidate_id)
  expect_identical(z$status,c("perpendicular","resolved_rotation","resolved_rotation","perpendicular"))
  expect_identical(sf::st_as_binary(sf::st_geometry(z$sections)),sf::st_as_binary(sf::st_geometry(
    cross_section_channel_deconflict(f$sections,f$channel,rep(TRUE,4),f$options,.001,30)$sections)))
})

test_that("infeasible envelopes and search budgets retain original flagged geometry", {
  f <- channel_search_fixture()
  original <- sf::st_as_binary(sf::st_geometry(f$sections))
  for(args in list(list(angle_limit=10),list(angle_limit=30,max_nodes=0L))) {
    z <- do.call(cross_section_channel_deconflict,c(list(sections=f$sections,channel=f$channel,
      eligible=rep(TRUE,4),options_for=f$options,tolerance=.001),args))
    expect_identical(sf::st_as_binary(sf::st_geometry(z$sections)),original)
    expect_true(all(z$status %in% c("no_feasible_grid","search_limit")))
  }
  z <- cross_section_channel_deconflict(f$sections,f$channel,rep(FALSE,4),function(i)stop("unused"),.001,30)
  expect_identical(sf::st_as_binary(sf::st_geometry(z$sections)),original)
})

test_that("the constructor repairs channel-only contacts without narrowing or losing stations", {
  polygon <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(0,0),c(100,0),c(100,100),c(0,100),c(0,0)))),crs=26915))
  line <- sf::st_sf(ReachName="test",geometry=sf::st_sfc(sf::st_linestring(rbind(c(10,50),c(90,50))),crs=26915))
  a <- cross_section_regions(line,polygon,polygon)
  sf::st_geometry(a$centerlines)[[which(a$centerlines$region=="channel")]] <-
    sf::st_linestring(rbind(c(0,50),c(35,52),c(45,48),c(55,49),c(65,51),c(100,50)))
  original <- cross_section_layout(line,polygon,polygon,tangent_distance_m=2,angle_limit_deg=0,region_artifacts=a)
  fixed <- cross_section_layout(line,polygon,polygon,tangent_distance_m=2,region_artifacts=a)
  expect_true(any(original$sections$channel_conflict))
  expect_false(any(fixed$sections$conflict | fixed$sections$channel_order_review))
  expect_true(any(fixed$sections$channel_rotated))
  expect_equal(fixed$sections$distance_m,original$sections$distance_m)
  for(i in seq_len(nrow(fixed$sections))) {
    xy <- sf::st_coordinates(fixed$sections[i,])[,1:2]
    expect_equal(unname(xy[,2]),c(0,100),tolerance=1e-6)
    center <- sf::st_sfc(sf::st_point(c(20+20*(i-1L),50)),crs=26915)
    expect_lt(as.numeric(sf::st_distance(center,fixed$sections[i,])),1e-6)
  }
  expect_true(fixed$parameters$allow_overbank_bends)
  expect_true(check_cross_section_layout(fixed$sections))
})

test_that("mapped overbanks do not exclude contacting channel segments from repair", {
  polygon <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(0,0),c(100,0),c(100,100),c(0,100),c(0,0)))),crs=26915))
  floodplain <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(0,-30),c(100,-30),c(100,130),c(0,130),c(0,-30)))),crs=26915))
  line <- sf::st_sf(ReachName="test",geometry=sf::st_sfc(sf::st_linestring(rbind(c(10,50),c(90,50))),crs=26915))
  a <- cross_section_regions(line,polygon,floodplain)
  sf::st_geometry(a$centerlines)[[which(a$centerlines$region=="channel")]] <-
    sf::st_linestring(rbind(c(0,50),c(35,52),c(45,48),c(55,49),c(65,51),c(100,50)))
  original <- cross_section_layout(line,polygon,floodplain,tangent_distance_m=2,angle_limit_deg=0,region_artifacts=a)
  fixed <- cross_section_layout(line,polygon,floodplain,tangent_distance_m=2,region_artifacts=a)
  expect_true(any(original$sections$channel_conflict))
  expect_false(any(fixed$sections$channel_conflict | fixed$sections$channel_order_conflict))
  expect_true(any(fixed$sections$channel_rotated))
  expect_true(any(fixed$sections$channel_rotated &
    (fixed$sections$left_leg_status!="no_floodplain" | fixed$sections$right_leg_status!="no_floodplain")))
  expect_identical(fixed$sections$distance_m,original$sections$distance_m)
})
