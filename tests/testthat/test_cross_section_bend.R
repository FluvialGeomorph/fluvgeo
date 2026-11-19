test_that("joint bend planning spreads clustered targets without changing banks or stations", {
  f <- layout_fixture();x <- cross_section_layout(f$line,f$channel,f$floodplain)
  s <- x$sections;a <- x$boundary_anchors
  rows <- which(a$role=="outer" & a$side=="left" & a$Seq %in% 3:5)
  rows <- rows[order(a$Seq[rows])]
  line <- sf::st_geometry(x$boundary_chains[match(a$chain_id[rows[1]],x$boundary_chains$chain_id),])
  m <- a$measure[rows[2]]+c(-.008,0,.008)
  a$measure[rows] <- m
  sf::st_geometry(a)[rows] <- sf::st_cast(sf::st_line_sample(line,sample=m/as.numeric(sf::st_length(line))),"POINT")
  for(j in seq_along(rows)) {
    i <- match(a$candidate_id[rows[j]],s$candidate_id)
    xy <- sf::st_coordinates(s[i,])[,1:2,drop=FALSE];xy[1,] <- sf::st_coordinates(a[rows[j],])[1,1:2]
    sf::st_geometry(s)[[i]] <- sf::st_linestring(xy)
  }
  bank_coordinates <- function(y)lapply(seq_len(nrow(y)),function(i) {
    unname(sf::st_coordinates(y[i,])[y$left_bank_vertex[i]:y$right_bank_vertex[i],1:2,drop=FALSE])
  })
  before <- bank_coordinates(s)
  z <- cross_section_bend_plan(s,x$regions,x$boundary_chains,a,.001,30)
  expect_gt(z$accepted,0)
  expect_gt(min(diff(z$anchors$measure[rows])),1)
  expect_equal(bank_coordinates(z$sections),before)
  expect_identical(z$sections$distance_m,s$distance_m)
  reviewed <- cross_section_layout_review(z$sections,f$floodplain,x$boundary_chains)
  expect_false(any(reviewed$conflict | reviewed$self_crossing | reviewed$left_order_conflict | reviewed$right_order_conflict))
  expect_lt(max(reviewed$outside_floodplain_m),1e-6)
  expect_true(all(reviewed$left_supported & reviewed$right_supported))
  factor <- 3937/1200
  feet <- function(x) {
    sf::st_geometry(x) <- sf::st_set_crs(sf::st_set_crs(sf::st_geometry(x),NA)*factor,2277)
    x
  }
  af <- feet(a);af$measure <- af$measure*factor;af$proposed_measure <- af$proposed_measure*factor
  zf <- cross_section_bend_plan(feet(s),feet(x$regions),feet(x$boundary_chains),af,.001*factor,30)
  expect_gt(zf$accepted,0)
  expect_gt(min(diff(zf$anchors$measure[rows])),factor)
  expect_identical(zf$sections$distance_m,s$distance_m)
  expect_equal(bank_coordinates(zf$sections),lapply(before,function(v)v*factor),tolerance=1e-6)
  expect_identical(sf::st_as_binary(sf::st_geometry(z$sections)),sf::st_as_binary(sf::st_geometry(
    cross_section_bend_plan(s,x$regions,x$boundary_chains,a,.001,30)$sections)))
})

test_that("bend quality penalizes backtracking without a section length objective", {
  route <- function(x)sf::st_sfc(sf::st_linestring(matrix(x,ncol=2,byrow=TRUE)))
  straight <- route(c(0,0,1,0));long <- route(c(0,0,100,0))
  bent <- route(c(0,0,1,1,2,0));backtrack <- route(c(0,0,1,1,0,0,2,0))
  expect_equal(cross_section_bend_quality(list(straight)),cross_section_bend_quality(list(long)))
  expect_gt(cross_section_bend_quality(list(backtrack)),cross_section_bend_quality(list(bent)))
})

test_that("shared mesh reuse preserves routes and respects changed obstacles", {
  rectangle <- function(x0,y0,x1,y1)sf::st_polygon(list(rbind(
    c(x0,y0),c(x1,y0),c(x1,y1),c(x0,y1),c(x0,y0))))
  polygon <- sf::st_difference(sf::st_sfc(rectangle(0,0,10,10),crs=26915),
    sf::st_sfc(rectangle(4,3,6,7),crs=26915))
  mesh <- cross_section_mesh(polygon)
  first <- cross_section_mesh_path(c(0,5),c(10,5),polygon,mesh,clearance=1)
  expect_false(is.null(first))
  expect_identical(sf::st_as_binary(first),sf::st_as_binary(
    cross_section_mesh_path(c(0,5),c(10,5),polygon,mesh,clearance=1)))
  wall <- sf::st_sfc(sf::st_linestring(rbind(c(2,0),c(2,5))),crs=26915)
  changed <- cross_section_mesh_path(c(0,5),c(10,5),polygon,mesh,wall,clearance=1)
  fresh <- cross_section_mesh_path(c(0,5),c(10,5),polygon,cross_section_mesh(polygon),wall,clearance=1)
  expect_equal(changed,fresh)
  if(!is.null(changed))expect_length(sf::st_intersects(changed,wall)[[1]],0L)
})

test_that("incumbent guides retain banks and obey current containment and barriers", {
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(0,0),c(20,0),c(20,10),c(0,10),c(0,0)))),crs=26915)
  original <- sf::st_sfc(sf::st_linestring(rbind(c(0,2),c(8,2),c(8,8),c(20,8))),crs=26915)
  before <- sf::st_as_binary(original)
  result <- cross_section_retarget_route(original,c(20,9),polygon,sf::st_sfc(crs=26915),.001)
  expect_false(is.null(result))
  xy <- sf::st_coordinates(result)[,1:2,drop=FALSE]
  expect_equal(unname(xy[1,]),c(0,2));expect_equal(unname(tail(xy,1)[1,]),c(20,9))
  expect_true(sf::st_is_simple(result));expect_true(sf::st_covered_by(result,polygon,sparse=FALSE)[1,1])
  wall <- sf::st_sfc(sf::st_linestring(rbind(c(19,0),c(19,10))),crs=26915)
  expect_null(cross_section_retarget_route(original,c(20,9),polygon,wall,.001))
  expect_identical(sf::st_as_binary(original),before)
})
