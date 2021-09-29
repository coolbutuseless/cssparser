

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Normalize a hex colour to an 8-char hex
#'
#' \itemize{
#' \item{If hex colour doesn't have an alpha channel, add it as 'FF'}
#' \item{If hex colour only has 3 digits, expand it to 6, then add alpha channel}
#' }
#'
#' @param hex_colour e.g. '#000'
#'
#' @return 8 char hex colour
#'
#'
#' @examples
#' \dontrun{
#' parse_colour_hex('#123') # -> '#112233FF'
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_colour_hex <- function(hex_colour) {

  if (!startsWith(hex_colour, '#')) {
    stop("Not a hex colour: ", hex_colour)
  }

  hex_colour <- toupper(hex_colour)

  if (nchar(hex_colour) == 7) {
    return(paste0(hex_colour, "FF"))
  } else if (nchar(hex_colour) == 9) {
    return(hex_colour)
  }

  bits <- strsplit(hex_colour, "")[[1]][-1]
  if (length(bits) == 3) {
    bits <- c(bits[1], bits[1], bits[2], bits[2], bits[3], bits[3], 'F', 'F')
  } else if (length(bits) == 4) {
    bits <- c(bits[1], bits[1], bits[2], bits[2], bits[3], bits[3], bits[4], bits[4])
  } else {
    stop("Not a valid hex colour: ", hex_colour)
  }

  paste(c('#', bits), collapse="")
}
