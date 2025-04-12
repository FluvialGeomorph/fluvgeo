test_that("leaflet map", {
  leaf <- get_leaflet()
  leaf
  expect_true("leaflet" %in% class(leaf))
})

test_that("leaflet map with search", {
  leaf_search <- get_leaflet(search = TRUE)
  leaf_search
  expect_true("leaflet" %in% class(leaf_search))
})
