layout_fixture <- function() {
  line <- sf::st_sf(ReachName = "R1", geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(200, 0))), crs = 26915))
  list(line = line,
    channel = sf::st_sf(geometry = sf::st_buffer(sf::st_geometry(line), 5, endCapStyle = "FLAT")),
    floodplain = sf::st_sf(geometry = sf::st_buffer(sf::st_geometry(line), 30, endCapStyle = "FLAT")))
}
