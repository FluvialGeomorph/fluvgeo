#' Extract line end points
#'
#' @description Determines the requested line end and returns a new sf point
#' object with x and y coordinate value attributes added. Also adds ReachName
#' XS Seq attributes suitable for joining.
#'
#' @param line       sf line object, The input line feature.
#'                   be calculated.
#' @param end        character, Which end of the line? One of "start" or "end".
#'
#' @return  sf point object with x and y coordinate value attributes
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom sf st_cast st_line_sample st_as_sf
#'
sf_line_end_point <- function(line, end) {
  if(end == "start") {
    sample = 0                      # special value for identifying line start
    field_names = c("x_start", "y_start")}
  if(end == "end") {
    sample = 1                      # special code for identifying line end
    field_names = c("x_end", "y_end")}

  end_point <- line %>%
    st_cast(., to = "LINESTRING", warn = FALSE, do_split = TRUE) %>%
    st_line_sample(., sample = sample) %>%
    st_as_sf() %>%
    st_cast(., to = "POINT") %>%
    mutate(ReachName = line$ReachName) %>%
    mutate(Seq = line$Seq) %>%
    # write attributes to sf object
    fluvgeo::sf_point_attributes(., field_names = field_names)
}
