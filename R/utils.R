
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Test if string is a single string with something in it
#'
#' @param x string
#' @return TRUE if string is single string with something in it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_char1 <- function(x) {

  x <- trimws(x)

  !is.null        (x)        &&
    !is.na        (x)        &&
    length        (x) == 1   &&
    is.character  (x)        &&
    nchar         (x) > 0
}



"%||%" <- function(x, y) { #nocov start
  if (is.null(x)) {
    y
  } else {
    x
  }
} #nocov end
