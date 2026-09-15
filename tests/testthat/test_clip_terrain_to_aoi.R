clip_fixture <- function(root, values = NULL) {
  r <- terra::rast(nrows=6,ncols=6,xmin=0,xmax=6,ymin=0,ymax=6,crs='EPSG:26914')
  terra::values(r) <- if (is.null(values)) c(NA,seq(-3.25,5.25,length.out=35)) else values
  terra::units(r) <- 'm'
  dem <- file.path(root,'source.tif')
  terra::writeRaster(r,dem,datatype='FLT8S')
  polygon <- sf::st_polygon(list(rbind(c(1.1,1.1),c(4.9,1.1),c(1.1,4.9),c(1.1,1.1))))
  list(dem=dem,aoi=sf::st_sf(label='explicit test AOI',geometry=sf::st_sfc(polygon,crs=26914)))
}

test_that('clipping executes and records without changing its input', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); hash <- .fg_file_sha256(f$dem)
  out <- clip_terrain_to_aoi(f$dem,f$aoi,file.path(root,'clip'),'Synthetic AOI',touches=FALSE)
  r <- terra::rast(out$terrain); source <- terra::rast(f$dem)
  expect_equal(unname(as.vector(terra::ext(r))),c(1,5,1,5))
  expect_equal(terra::res(r),terra::res(source))
  expect_equal(terra::origin(r),terra::origin(source))
  expect_equal(terra::crs(r),terra::crs(source))
  expect_equal(terra::units(r),'m')
  # Independent center-point membership and source-cell lookup, not a second mask call.
  xy <- terra::xyFromCell(r,seq_len(terra::ncell(r)))
  keep <- lengths(sf::st_intersects(sf::st_as_sf(data.frame(x=xy[,1],y=xy[,2]),
    coords=c('x','y'),crs=26914),f$aoi)) > 0L
  expected <- terra::values(source)[terra::cellFromXY(source,xy)]
  expected[!keep] <- NA_real_
  expect_equal(as.vector(terra::values(r)),expected,tolerance=0)
  expect_identical(.fg_file_sha256(f$dem),hash)
  receipt <- jsonlite::read_json(out$execution,simplifyVector=TRUE)
  expect_identical(receipt$status,'SUCCEEDED')
  expect_identical(receipt$operation,'CLIP_MASK_TERRAIN')
  expect_identical(receipt$input$sha256,hash)
  expect_true(all(unlist(receipt$checks)))
  expect_false(receipt$parameters$touches)
  expect_true(all(inspect_terrain_folder(out$manifest)$artifacts$hash_verified))
  expect_identical(jsonlite::read_json(file.path(root,'clip','started.json'))$status,'STARTED')
  expect_null(out$report)
  expect_false(file.exists(file.path(root,'clip','failure.json')))
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,dirname(out$terrain),'Again'),'new output folder')
  expect_identical(.fg_file_sha256(f$dem),hash)
})

test_that('boundary choices, holes and intentional NoData stay explicit', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root)
  a <- clip_terrain_to_aoi(f$dem,f$aoi,file.path(root,'touch'),'Test',TRUE)
  b <- clip_terrain_to_aoi(f$dem,f$aoi,file.path(root,'center'),'Test',FALSE)
  va <- terra::values(terra::rast(a$terrain)); vb <- terra::values(terra::rast(b$terrain))
  expect_true(sum(!is.na(va)) > sum(!is.na(vb)))
  expect_equal(va[!is.na(vb)],vb[!is.na(vb)],tolerance=0)
  ring <- rbind(c(0,0),c(6,0),c(6,6),c(0,6),c(0,0))
  hole <- rbind(c(2,2),c(2,4),c(4,4),c(4,2),c(2,2))
  f$aoi <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(ring,hole)),crs=26914))
  out <- clip_terrain_to_aoi(f$dem,f$aoi,file.path(root,'hole'),'Test',FALSE)
  v <- as.vector(terra::values(terra::rast(out$terrain)))
  expect_true(all(is.na(v[c(1,15,16,21,22)])))
  expect_equal(v[-c(1,15,16,21,22)],terra::values(terra::rast(f$dem))[-c(1,15,16,21,22)],tolerance=0)
  all_na <- file.path(root,'empty'); dir.create(all_na)
  g <- clip_fixture(all_na,rep(NA_real_,36))
  result <- clip_terrain_to_aoi(g$dem,g$aoi,file.path(root,'masked'),'Intentional mask')
  expect_true(all(is.na(terra::values(terra::rast(result$terrain)))))
  expect_identical(jsonlite::read_json(result$execution)$status,'SUCCEEDED')
  expect_no_error(terrain_clip_report(result$execution,file.path(root,'all-masked.html')))
})

test_that('unsupported or ambiguous inputs fail before output creation', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); out <- file.path(root,'bad')
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,out,''),'rationale')
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,out,'Test',touches=NA),'logical')
  expect_error(clip_terrain_to_aoi(f$dem,sf::st_transform(f$aoi,4326),out,'Test'),'DEM CRS')
  large <- f$aoi; sf::st_geometry(large) <- sf::st_geometry(large)+10
  sf::st_crs(large) <- sf::st_crs(f$aoi)
  expect_error(clip_terrain_to_aoi(f$dem,large,out,'Test'),'beyond the DEM extent')
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi[0,],out,'Test'),'nonempty AOI')
  expect_false(dir.exists(out))
  aux <- paste0(f$dem,'.aux.xml'); writeLines('<PAMDataset/>',aux)
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,out,'Test'),'auxiliary files')
  expect_false(dir.exists(out))
})

test_that('serialization failures leave a failed receipt and no successful receipt', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); out <- file.path(root,'failed')
  local_mocked_bindings(.fg_clip_verify_values=function(...) stop('injected verification failure'))
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,out,'Test'),'injected verification failure')
  expect_true(file.exists(file.path(out,'started.json')))
  expect_identical(jsonlite::read_json(file.path(out,'failure.json'))$status,'FAILED')
  expect_false(file.exists(file.path(out,'execution.json')))
})

test_that('known vertical references are refused rather than discarded', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); compound <- file.path(root,'compound.tif')
  sf::gdal_utils('translate',f$dem,compound,options=c('-a_srs','EPSG:26914+5703',
    '-co','GEOTIFF_VERSION=1.0'),quiet=TRUE)
  before <- .fg_file_sha256(compound)
  expect_error(clip_terrain_to_aoi(compound,f$aoi,file.path(root,'out'),'Test'),'compound/3D')
  expect_identical(.fg_file_sha256(compound),before)
  expect_false(dir.exists(file.path(root,'out')))
})

test_that('a changed retained AOI cannot produce a successful receipt', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); out <- file.path(root,'changed')
  local_mocked_bindings(.fg_clip_verify_values=function(...) {
    writeLines('changed during run',file.path(out,'aoi.gpkg'))
  })
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,out,'Test'),'Source or retained AOI changed')
  expect_false(file.exists(file.path(out,'execution.json')))
  expect_identical(jsonlite::read_json(file.path(out,'failure.json'))$status,'FAILED')
})

test_that('unknown units, scale/offset and multiple polygons preserve decoded values', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); source <- terra::rast(f$dem)
  terra::units(source) <- ''
  plain <- file.path(root,'plain.tif'); terra::writeRaster(source,plain,datatype='FLT8S')
  scaled <- file.path(root,'scaled.tif')
  sf::gdal_utils('translate',plain,scaled,options=c('-a_scale','2','-a_offset','100'),quiet=TRUE)
  out <- clip_terrain_to_aoi(scaled,rbind(f$aoi,f$aoi),file.path(root,'out'),'Test',FALSE)
  expected <- terra::mask(terra::crop(terra::rast(scaled),terra::vect(f$aoi),snap='out'),
    terra::vect(f$aoi),touches=FALSE)
  expect_equal(terra::values(terra::rast(out$terrain)),terra::values(expected),ignore_attr=TRUE,tolerance=0)
  expect_identical(jsonlite::read_json(out$execution)$output$reference$band_unit,'')
  expect_equal(nrow(sf::st_read(out$aoi,quiet=TRUE)),2L)
})

test_that('render failure preserves a successful run for read-only retry', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root); folder <- file.path(root,'out')
  local_mocked_bindings(terrain_clip_report=function(...) stop('injected report failure'))
  expect_error(clip_terrain_to_aoi(f$dem,f$aoi,folder,'Test',report=TRUE),'injected report failure')
  expect_identical(jsonlite::read_json(file.path(folder,'execution.json'))$status,'SUCCEEDED')
  expect_true(file.exists(file.path(folder,'terrain.tif')))
  expect_false(file.exists(file.path(folder,'failure.json')))
})

test_that('reporting checks integrity and does not execute another clipping operation', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- clip_fixture(root)
  out <- clip_terrain_to_aoi(f$dem,f$aoi,file.path(root,'run'),'<script>not code</script>')
  baseline <- .fg_file_sha256(out$terrain)
  html <- file.path(root,'report.html')
  expect_no_error(terrain_clip_report(out$execution,html))
  text <- paste(readLines(html,warn=FALSE),collapse='\n')
  expect_match(text,'No CSV history entry is needed',fixed=TRUE)
  expect_match(text,'&lt;script&gt;not code&lt;/script&gt;',fixed=TRUE)
  expect_false(grepl('<script>not code</script>',text,fixed=TRUE))
  expect_identical(.fg_file_sha256(out$terrain),baseline)
  expect_error(terrain_clip_report(out$execution,html),'new .html')
  moved <- file.path(root,'relocated'); dir.create(moved)
  expect_true(all(file.copy(list.files(dirname(out$terrain),full.names=TRUE),moved)))
  expect_no_error(terrain_clip_report(file.path(moved,'execution.json'),file.path(root,'moved.html')))
  writeLines('changed',file.path(moved,'aoi.gpkg'))
  expect_error(terrain_clip_report(file.path(moved,'execution.json'),file.path(root,'bad.html')),'missing or changed')
})
