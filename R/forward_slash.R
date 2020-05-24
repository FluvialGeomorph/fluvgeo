#' @title Convert Windows path to forward slashes
#'
#' @export
#' @description Convert a Windows file path from double backslashes to single
#' forward slash file seperators suitable for use in R.
#'
#' @param path        character; A windows file path containing escaped
#'                    backslashs (i.e., \\)
#'
forward_slash <- function(path) {
  path <- gsub("\\\\", "/", path)
}
