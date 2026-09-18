navigation_lines <- function() {
  # 30 is the downstream origin. 20 is its upstream junction approach;
  # 10->11 and 90 are tributaries. IDs intentionally differ from traversal order.
  sf::st_sf(nhdplus_comid=c("10","11","20","30","90","99"),geometry=sf::st_sfc(
    sf::st_linestring(rbind(c(-90.02,40.03),c(-90.01,40.02))),
    sf::st_linestring(rbind(c(-90.01,40.02),c(-90,40.01))),
    sf::st_linestring(rbind(c(-90,40.01),c(-90,40))),
    sf::st_linestring(rbind(c(-90,40),c(-90,39.99))),
    sf::st_linestring(rbind(c(-89.99,40.02),c(-90,40.01))),
    sf::st_linestring(rbind(c(-91,40),c(-91,39.99))),crs=4326))
}
test_that("navigation follows direction and keeps tributary branches together", {
  x <- navigation_lines(); original <- x
  up <- order_drainage_flowlines(x,"30","upstream")
  expect_equal(up$source_id,c("30","20","11","10","90","99"))
  expect_equal(up$order_status,c(rep("ordered",5),"unreached"))
  expect_equal(up$navigation_order,c(1:5,NA_integer_))
  down <- order_drainage_flowlines(x,"10","downstream")
  expect_equal(down$source_id[down$order_status=="ordered"],c("10","11","20","30"))
  branch <- order_drainage_flowlines(x,"11","upstream")
  expect_equal(branch$source_id[branch$order_status=="ordered"],c("11","10"))
  expect_identical(x,original)
  shuffled <- x[c(6,4,1,5,3,2),]
  expect_equal(order_drainage_flowlines(shuffled,"30")$source_id,up$source_id)
  expect_equal(order_drainage_flowlines(sf::st_transform(x,3857),"30")$source_id,up$source_id)
})
test_that("unresolved features remain present without invented navigation", {
  x <- navigation_lines()
  expect_true(all(order_drainage_flowlines(x,"absent")$order_status=="unresolved"))
  bad <- x; bad$nhdplus_comid[2] <- bad$nhdplus_comid[1]
  expect_equal(nrow(order_drainage_flowlines(bad,"30")),nrow(x))
  expect_true(all(order_drainage_flowlines(bad,"30")$order_status=="unresolved"))
  loop <- x[1:3,]
  sf::st_geometry(loop) <- sf::st_sfc(
    sf::st_linestring(rbind(c(0,0),c(0,1))),sf::st_linestring(rbind(c(0,1),c(1,1))),
    sf::st_linestring(rbind(c(1,1),c(0,0))),crs=4326)
  expect_true(all(order_drainage_flowlines(loop,"10")$order_status=="unresolved"))
})

test_that("whole-network order is downstream to upstream independent of origin and row order", {
  x <- navigation_lines()
  walk <- order_drainage_flowlines(x)
  rank <- setNames(walk$navigation_order,walk$source_id)
  expect_true(rank[["30"]] < rank[["20"]])
  expect_true(rank[["20"]] < rank[["11"]])
  expect_true(rank[["11"]] < rank[["10"]])
  expect_true(rank[["20"]] < rank[["90"]])
  expect_equal(order_drainage_flowlines(x[c(6,4,1,3,5,2),])$source_id,walk$source_id)
  expect_equal(order_drainage_flowlines(sf::st_transform(x,26915))$source_id,walk$source_id)
  expect_setequal(walk$source_id,x$nhdplus_comid)
})
