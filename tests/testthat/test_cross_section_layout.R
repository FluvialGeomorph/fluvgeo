test_that("straight-channel candidates have independent full-length overbanks and correct direction", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line, f$channel, f$floodplain)
  expect_equal(x$sections$distance_m, seq(10, 190, 20))
  expect_equal(x$sections$Seq, 1:10)
  expect_true(all(x$sections$oriented))
  expect_false(any(x$sections$conflict))
  expect_lt(max(abs(sf::st_coordinates(x$sections[3, ])[, 1:2]-
    cbind(X = rep(50, 4), Y = c(-30, -5, 5, 30)))),1)
  expect_equal(x$sections$left_boundary_gap_m, rep(0, 10), tolerance = 1e-6)
  expect_equal(x$sections$outside_floodplain_m, rep(0, 10), tolerance = 1e-6)
  expect_setequal(x$paths$region, c("channel","left_floodplain", "right_floodplain"))
  expect_false(any(x$samples$inferred[x$samples$distance_m>=30 & x$samples$distance_m<=170]))
  expect_equal(nrow(x$region_polygons),3)
  expect_true(check_cross_section_layout(x$sections))
})

test_that("candidate validation preserves exact fields, types, endpoints and explicit metadata", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line,f$channel,f$floodplain)$sections
  renamed <- x;names(renamed)[names(renamed)=="ReachName"] <- "reach_name"
  expect_error(check_cross_section_layout(renamed),"exact")
  mistyped <- x;mistyped$Seq <- as.character(mistyped$Seq)
  expect_error(check_cross_section_layout(mistyped),"integer")
  wrong <- x;wrong$x_start <- wrong$x_start+1
  expect_error(check_cross_section_layout(wrong),"Endpoint")
  x$layout_spacing_m <- 20;x$layout_direction_window_m <- 80
  x$channel_rem_elevation <- 103;x$floodplain_rem_elevation <- 106
  x$rem_reference_elevation <- 100;x$rem_elevation_units <- "ft"
  x$layout_width_mode <- "floodplain_extent";x$layout_method <- "experiment-v1"
  expect_true(check_cross_section_layout(x,"candidate_table"))
  wrong <- x;wrong$rem_elevation_units <- "unknown"
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"REM units")
  wrong <- x;wrong$channel_rem_elevation <- 107
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"below floodplain")
  x$layout_method <- "polygon-centerline-v3"
  x$layout_tangent_distance_m <- 20;x$layout_centerline_spacing_m <- 2
  x$layout_smoothing_refinements <- 2L;x$layout_overbank_angle_limit_deg <- 30
  expect_true(check_cross_section_layout(x,"candidate_table"))
  wrong <- x;wrong$left_leg_status[1] <- "looks_ok"
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"recognized status")
  wrong <- x;wrong$left_rotation_deg[1] <- 31
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"declared bound")
  x$layout_method<-"polygon-centerline-v4";x$layout_allow_overbank_bends<-TRUE
  expect_true(check_cross_section_layout(x,"candidate_table"))
  wrong<-x;wrong$layout_allow_overbank_bends<-NA
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"Declare whether")
  x$layout_method <- "polygon-centerline-v6"
  expect_true(check_cross_section_layout(x,"candidate_table"))
  wrong <- x;wrong$left_order_conflict <- NULL
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"boundary-order")
  x$layout_method <- "polygon-centerline-v8"
  x$layout_channel_angle_limit_deg <- 30;x$layout_channel_rotation_review_deg <- 15
  expect_true(check_cross_section_layout(x,"candidate_table"))
  wrong <- x;wrong$layout_channel_angle_limit_deg <- NULL
  expect_error(check_cross_section_layout(wrong,"candidate_table"),"angular search")
})

test_that("absent mapped overbanks yield valid two-vertex channel cross sections", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line, f$channel, f$channel)
  expect_equal(nrow(x$sections), 10)
  expect_false(any(x$sections$left_inferred | x$sections$right_inferred))
  expect_false(any(x$sections$left_supported | x$sections$right_supported))
  expect_true(all(x$sections$left_leg_status=="no_floodplain"))
  expect_true(all(x$sections$right_leg_status=="no_floodplain"))
  expect_false(any(x$samples$inferred))
  expect_equal(sum(x$paths$region!="channel"), 0)
  expect_true(all(vapply(sf::st_geometry(x$sections),nrow,integer(1))==2L))
  expect_identical(x$sections$left_bank_vertex,rep(1L,10))
  expect_identical(x$sections$right_bank_vertex,rep(2L,10))
  expect_true(all(x$sections$section_parts=="channel_only"))
  expect_false(any(x$sections$conflict | x$sections$channel_skewed))
  expect_true(all(x$sections$left_bend_count==0L & x$sections$right_bend_count==0L))
  expect_true(check_cross_section_layout(x$sections))
  wrong <- x$sections;wrong$left_leg_status[1] <- "complete"
  expect_error(check_cross_section_layout(wrong),"Leg status")
  wrong <- x$sections;wrong$left_leg_status <- NULL
  expect_error(check_cross_section_layout(wrong),"Declare leg status")
  wrong <- x$sections;wrong$layout_method <- "polygon-centerline-v6"
  expect_error(check_cross_section_layout(wrong),"Historical")
  wrong <- x$sections;wrong$layout_method <- "polygon-centerline-v7"
  xy <- sf::st_coordinates(wrong[1,])[,1:2]
  sf::st_geometry(wrong)[[1]] <- sf::st_linestring(xy[c(1,1,2),])
  wrong$left_bank_vertex[1] <- 2L;wrong$right_bank_vertex[1] <- 3L
  expect_error(check_cross_section_layout(wrong),"duplicated adjacent")
})

test_that("either overbank can be absent without fabricating an edge", {
  f <- layout_fixture()
  for(side in c("left","right")) {
    bounds <- if(side=="left")c(-30,5) else c(-5,30)
    fp <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
      c(0,bounds[1]),c(200,bounds[1]),c(200,bounds[2]),c(0,bounds[2]),c(0,bounds[1])))),crs=26915))
    x <- cross_section_layout(f$line,f$channel,fp)
    expect_true(all(vapply(sf::st_geometry(x$sections),nrow,integer(1))==3L))
    expect_true(all(x$sections$section_parts==paste0("channel_",side,"_overbank")))
    expect_true(all(x$sections[[paste0(side,"_leg_status")]]=="complete"))
    expect_true(all(x$sections[[paste0(if(side=="left")"right" else "left","_leg_status")]]=="no_floodplain"))
    expect_false(any(x$sections$conflict))
    expect_true(check_cross_section_layout(x$sections))
    wrong <- x$sections;wrong[[paste0(side,"_leg_status")]][1] <- "no_floodplain"
    expect_error(check_cross_section_layout(wrong),"Leg status")
  }
})

test_that("failed overbank construction stays distinct from legitimate absence", {
  f <- layout_fixture();a <- cross_section_regions(f$line,f$channel,f$floodplain)
  a$centerlines <- a$centerlines[a$centerlines$region=="channel",]
  x <- cross_section_layout(f$line,f$channel,f$floodplain,region_artifacts=a)
  expect_true(all(x$sections$left_leg_status!="no_floodplain"))
  expect_true(all(x$sections$right_leg_status!="no_floodplain"))
  expect_false(any(x$sections$left_supported | x$sections$right_supported))
  expect_true(all(x$sections$left_boundary_gap_m[3:8]>20))
  expect_true(check_cross_section_layout(x$sections))
})

test_that("channel-only review retains contacts and historical duplicated banks remain readable", {
  f <- layout_fixture();x <- cross_section_layout(f$line,f$channel,f$channel)$sections
  sf::st_geometry(x)[[2]] <- sf::st_geometry(x)[[1]]
  x <- cross_section_layout_review(x,f$channel)
  expect_true(all(x$channel_conflict[1:2]))
  expect_true(all(x$conflict[1:2]))
  expect_false(any(x$left_conflict | x$right_conflict))
  old <- x[1,];old$Seq <- 1L;old$left_bank_vertex <- 2L;old$right_bank_vertex <- 3L
  xy <- sf::st_coordinates(old)[,1:2];sf::st_geometry(old)[[1]] <- sf::st_linestring(xy[c(1,1,2,2),])
  expect_true(check_cross_section_layout(cross_section_layout_review(old,f$channel)))
})

test_that("five and six vertices preserve bank identity, endpoint fields and review", {
  f <- layout_fixture();x<-cross_section_layout(f$line,f$channel,f$floodplain)$sections[3,];x$Seq<-1L
  original <- sf::st_coordinates(x)[,1:2]
  xy <- rbind(original[1,],c(48,-15),original[2:3,],c(52,15),original[4,])
  sf::st_geometry(x)[[1]] <- sf::st_linestring(xy)
  x$left_bank_vertex<-3L;x$right_bank_vertex<-4L;x$left_bend_added<-TRUE;x$right_bend_added<-TRUE
  reviewed <- cross_section_layout_review(x,f$floodplain)
  expect_true(check_cross_section_layout(reviewed))
  expect_equal(c(reviewed$x_start,reviewed$y_start,reviewed$x_end,reviewed$y_end),unname(c(original[1,],original[4,])))
  expect_true(reviewed$oriented)
  wrong<-reviewed;wrong$right_bank_vertex<-5L
  expect_error(check_cross_section_layout(wrong),"Bank vertex")
  undeclared<-reviewed;undeclared$left_bank_vertex<-NULL;undeclared$right_bank_vertex<-NULL
  expect_error(check_cross_section_layout(undeclared),"Declare bank")
  sf::st_geometry(x)[[1]]<-sf::st_linestring(xy[-5,]);x$right_bend_added<-FALSE
  expect_true(check_cross_section_layout(cross_section_layout_review(x,f$floodplain)))
  xy<-rbind(original[1,],c(46,-22),c(48,-15),original[2:3,],c(52,15),c(54,22),original[4,])
  sf::st_geometry(x)[[1]]<-sf::st_linestring(xy);x$left_bank_vertex<-4L;x$right_bank_vertex<-5L
  multi<-cross_section_layout_review(x,f$floodplain)
  expect_true(check_cross_section_layout(multi))
  expect_equal(c(multi$left_bend_count,multi$right_bend_count),c(2L,2L))
})

test_that("a blocked normal finds a contained full-coverage rotated alternative", {
  f <- layout_fixture()
  # A locally sloping channel bank makes a centerline-normal ray initially
  # point into the channel. A modest rotation can still reach the outer edge.
  polygon <- function(xy)sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(xy,xy[1,]))),crs=26915))
  f$channel <- polygon(rbind(c(0,-5),c(45,-5),c(55,-25),c(65,-5),c(200,-5),c(200,5),c(0,5)))
  f$floodplain <- polygon(rbind(c(0,-50),c(200,-50),c(200,30),c(0,30)))
  # This fixture isolates rotation around a deliberately retained channel arm.
  a <- cross_section_regions(f$line,f$channel,f$floodplain,prepare_channel=FALSE)
  sf::st_geometry(a$centerlines)[[which(a$centerlines$region=="channel")]] <- sf::st_geometry(f$line)[[1]]
  j <- which(a$centerlines$region=="left_floodplain")
  sf::st_geometry(a$centerlines)[[j]] <- sf::st_linestring(rbind(c(0,-45),c(40,-40),c(60,-20),c(200,-40)))
  before <- sf::st_as_binary(sf::st_geometry(a$centerlines))
  x <- cross_section_layout(f$line,f$channel,f$floodplain,region_artifacts=a,angle_limit_deg=0)
  expect_false(x$parameters$prepare_channel)
  expect_true(all(x$sections$left_supported))
  expect_true(any(x$sections$left_visibility_adjusted | abs(x$sections$left_rotation_deg)>0))
  expect_identical(sf::st_as_binary(sf::st_geometry(x$paths)),before)
  samples <- x$samples[x$samples$side=="left",]
  connectors <- sf::st_sfc(lapply(seq_len(nrow(x$sections)),function(i)
    sf::st_linestring(rbind(sf::st_coordinates(x$sections[i,])[x$sections$left_bank_vertex[i],1:2],
      sf::st_coordinates(samples[i,])[1,1:2]))),crs=26915)
  expect_true(all(lengths(sf::st_covered_by(connectors,sf::st_buffer(
    sf::st_geometry(a$regions[a$regions$region=="left_floodplain",]),.001)))==1L))
  expect_lte(max(x$sections$outside_floodplain_m),.001)
  expect_lte(max(x$sections$left_boundary_gap_m),.001)
  expect_true(all(abs(x$sections$left_rotation_deg)<=30))
})

test_that("review finds nonadjacent touching/crossing lines without altering geometry", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line, f$channel, f$floodplain)$sections
  xy <- sf::st_coordinates(x[3, ])[, 1:2]
  xy[1, ] <- sf::st_coordinates(x[1, ])[1, 1:2]
  sf::st_geometry(x)[[3]] <- sf::st_linestring(xy)
  before <- sf::st_as_binary(sf::st_geometry(x))
  reviewed <- cross_section_layout_review(x, f$floodplain)
  expect_true(reviewed$conflict[1])
  expect_true(reviewed$conflict[3])
  expect_match(reviewed$conflicts_with[1], "3")
  expect_identical(sf::st_as_binary(sf::st_geometry(reviewed)), before)
  xy[1, ] <- c(50, -15)
  sf::st_geometry(x)[[3]] <- sf::st_linestring(xy)
  reviewed <- cross_section_layout_review(x, f$floodplain)
  expect_equal(reviewed$left_boundary_gap_m[3], 15, tolerance = 1e-6)
  expect_equal(reviewed$right_boundary_gap_m[3], 0, tolerance = 1e-6)
  expect_false(reviewed$conflict[1])
  expect_equal(reviewed$x_start[3], 50)
})

test_that("edits expose reversed orientation and out-of-footprint geometry", {
  f <- layout_fixture()
  x <- cross_section_layout(f$line, f$channel, f$floodplain)$sections[1, ]
  xy <- rbind(c(10,40),c(10,5),c(10,-5),c(10,-30))
  sf::st_geometry(x)[[1]] <- sf::st_linestring(xy)
  reviewed <- cross_section_layout_review(x, f$floodplain)
  expect_false(reviewed$oriented)
  expect_equal(reviewed$outside_floodplain_m, 10, tolerance = 1e-6)
  invalid<-x;invalid$left_bank_vertex<-0L
  expect_error(cross_section_layout_review(invalid, f$floodplain), "Bank vertex")
})

test_that("invalid units, geometry and nesting fail explicitly", {
  f <- layout_fixture()
  expect_error(cross_section_layout(f$line, f$channel, f$floodplain, spacing_m = 0), "positive")
  expect_error(cross_section_layout(f$line, f$channel, f$floodplain, angle_limit_deg = 90), "angle")
  expect_error(cross_section_layout(sf::st_transform(f$line, 4326), f$channel, f$floodplain), "projected")
  expect_error(cross_section_layout(f$line, f$floodplain, f$channel), "contained")
  feet <- suppressWarnings(lapply(f, sf::st_set_crs, value = 3435))
  x <- cross_section_layout(feet$line, feet$channel, feet$floodplain)
  expect_equal(x$sections$distance_m[1:3], c(10, 30, 50))
  far_channel <- sf::st_set_geometry(f$channel,sf::st_geometry(f$channel)+c(0,100))
  sf::st_crs(far_channel) <- sf::st_crs(f$line)
  far_floodplain <- sf::st_set_geometry(f$floodplain,sf::st_geometry(f$floodplain)+c(0,100))
  sf::st_crs(far_floodplain) <- sf::st_crs(f$line)
  empty <- cross_section_layout(f$line,far_channel,far_floodplain)
  expect_equal(nrow(empty$sections),0)
  expect_equal(nrow(empty$failures),10)
})
