test_that("polygon routing can place multiple bends away from a centerline", {
  rectangle<-function(x0,y0,x1,y1)sf::st_polygon(list(rbind(c(x0,y0),c(x1,y0),c(x1,y1),c(x0,y1),c(x0,y0))))
  outer<-sf::st_sfc(rectangle(0,0,10,10),crs=26915)
  polygon<-sf::st_difference(outer,sf::st_sfc(rectangle(3,1,7,9),crs=26915))
  route<-cross_section_route_leg(c(0,5),c(10,5),polygon,sf::st_boundary(outer),sf::st_sfc(crs=26915),.001)
  expect_false(is.null(route))
  xy<-sf::st_coordinates(route)[,1:2,drop=FALSE]
  expect_gte(nrow(xy),4)
  expect_equal(unname(xy[c(1,nrow(xy)),]),rbind(c(0,5),c(10,5)))
  expect_true(sf::st_is_simple(route))
  expect_lt(sum(as.numeric(sf::st_length(sf::st_difference(route,polygon)))),.001)
  expect_true(any(abs(xy[-c(1,nrow(xy)),2]-5)>1))
  target <- c(10+1e-10,5)
  fixed <- cross_section_route_leg(c(0,5),target,polygon,sf::st_boundary(outer),
    sf::st_sfc(crs=26915),.001,fixed_target=TRUE)
  expect_false(is.null(fixed))
  expect_equal(unname(tail(sf::st_coordinates(fixed)[,1:2],1)[1,]),target,tolerance=1e-12)
})

test_that("routing avoids another section without bridging disconnected polygons", {
  polygon<-sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(10,0),c(10,10),c(0,10),c(0,0)))),crs=26915)
  obstacle<-sf::st_sfc(sf::st_linestring(rbind(c(8,0),c(2,10))),crs=26915)
  route<-cross_section_route_leg(c(2,0),c(8,10),polygon,sf::st_boundary(polygon),obstacle,.001)
  expect_false(is.null(route))
  expect_length(sf::st_intersects(route,obstacle)[[1]],0)
  expect_lt(as.numeric(sf::st_distance(sf::st_sfc(sf::st_point(tail(sf::st_coordinates(route)[,1:2],1)[1,]),crs=26915),sf::st_boundary(polygon))),.001)
})
