test_that("fixes 4326 incorrecly set to 3857", {
  # Features saved from mapedit module
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  sf::st_crs(xs_mapedit) <- 3857        # simulates mapedit::editMod crs = 3857
  obj <- xs_mapedit
  sf::st_crs(obj)$epsg                  # 3857, but this is incorrect, see bbox
  sf::st_bbox(obj)                      # bbox is latlon dd
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 4326, this is now correct, see bbox
  sf::st_bbox(fixed_obj)                # bbox is latlon dd
  expect_equal(sf::st_crs(fixed_obj)$epsg, 4326)
  # Now let's try a transform from 4326 to 3857
  xs_3857 <- sf::st_transform(fixed_obj, crs = 3857)
  sf::st_crs(xs_3857)$epsg              # 3857, correct, see bbox
  sf::st_bbox(xs_3857)                  # bbox is now meters
  expect_equal(sf::st_crs(xs_3857)$epsg, 3857)
})
test_that("fixes 4326 incorrecly set to 3857", {
  # Features saved from mapedit module
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  sf::st_crs(fl_mapedit) <- 3857        # simulates mapedit::editMod crs = 3857
  obj <- fl_mapedit
  sf::st_crs(obj)$epsg                  # 3857, but this is incorrect, see bbox
  sf::st_bbox(obj)                      # bbox is latlon dd
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 4326, this is now correct, see bbox
  sf::st_bbox(fixed_obj)                # bbox is meters
  expect_equal(sf::st_crs(fixed_obj)$epsg, 4326)
  # Now let's try a transform from 4326 to 3857
  fl_3857 <- sf::st_transform(fixed_obj, crs = 3857)
  sf::st_crs(fl_3857)$epsg              # 3857, correct, see bbox
  sf::st_bbox(fl_3857)                  # bbox is now meters
  expect_equal(sf::st_crs(fl_3857)$epsg, 3857)
})
test_that("fixes 3857 incorrectly set to 4326", {
  xs <- sf::st_read(system.file("extdata", "shiny", "xs.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  sf::st_crs(xs) <- 4326                # hypothetical case, not observed
  obj <- xs
  sf::st_crs(obj)$epsg                  # 4326, but this is incorrect, see bbox
  sf::st_bbox(obj)                      # bbox is meters
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 3857, this is now correct, see bbox
  sf::st_bbox(fixed_obj)                # bbox is meters
  expect_equal(sf::st_crs(fixed_obj)$epsg, 3857)
  # Now let's try a transform from 3857 to 4326
  xs_4326 <- sf::st_transform(fixed_obj, crs = 4326)
  sf::st_crs(xs_4326)$epsg              # 3857, correct, see bbox
  sf::st_bbox(xs_4326)                  # bbox is now meters
  expect_equal(sf::st_crs(xs_4326)$epsg, 4326)
})
test_that("handles 3857 already set correctly", {
  xs <- sf::st_read(system.file("extdata", "shiny", "xs.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  obj <- xs
  sf::st_crs(obj)$epsg                  # 3857, see bbox
  sf::st_bbox(obj)                      # bbox is meters
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 3857, see bbox
  sf::st_bbox(fixed_obj)                # bbox is meters
  expect_equal(sf::st_crs(xs)$epsg, sf::st_crs(fixed_obj)$epsg)
  # Now let's try a transform from 3857 to 4326
  xs_4326 <- sf::st_transform(fixed_obj, crs = 4326)
  sf::st_crs(xs_4326)$epsg              # 3857, correct, see bbox
  sf::st_bbox(xs_4326)                  # bbox is now meters
  expect_equal(sf::st_crs(xs_4326)$epsg, 4326)
})
test_that("handles 4326 already set correctly", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  obj <- xs_mapedit
  sf::st_crs(obj)$epsg                  # 4326, see bbox
  sf::st_bbox(obj)                      # bbox is latlon dd
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 4326, see bbox
  sf::st_bbox(fixed_obj)                # bbox is latlon dd
  expect_equal(sf::st_crs(xs_mapedit)$epsg, sf::st_crs(fixed_obj)$epsg)
  # Now let's try a transform from 4326 to 3857
  xs_3857 <- sf::st_transform(fixed_obj, crs = 3857)
  sf::st_crs(xs_3857)$epsg              # 3857, correct, see bbox
  sf::st_bbox(xs_3857)                  # bbox is now meters
  expect_equal(sf::st_crs(xs_3857)$epsg, 3857)
})
