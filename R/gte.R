#' @title Assign Vector Values Greater Than or Equal to Value
#'
#' @description Returns a numeric vector greater than or equal to the `value`
#' parameter. Sets the values less than `value` to the value of `value`.
#'
#' @param x       numeric vector; The vector of values to be adjusted.
#' @param value   numeric;
#'
#' @details Used to limit the range of values from a FluvialGeomorph metric to
#' ensure that only conceptually relevant values are returned. This is simply a
#' convenience function that uses a more computationally efficient method than
#' the available `ifelse` functions.
#'
#' @return Returns a numeric vector greater than or equal to the `value`
#' parameter. Sets the values less than `value` to the value of `value`.
#'
#' @importFrom assertthat assert_that
#'
gte <- function(x, value) {
  assert_that(is.numeric(x) & is.vector(x),
              msg = "x must be a numeric vector")
  assert_that(is.numeric(value),
              msg = "x must be a numeric")

  x[x <= value] <- value

  return(x)
}
