

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert CSS colour to standard hex colour (with alpha0)
#'
#' CSS colours can be lots of things:
#'
#' \itemize{
#' \item{Hex colour with 3, 6 or 8 characters}
#' \item{CSS colour name e.g. 'red'.  Not all CSS colours correspond to
#'       R colours e.g. 'silver' is not in R}
#' \item{rgb(), hsl(), hcl(), lab() and other colourspace-specific colour constructores.
#'       Only rgb and hsl are currently handled}
#' \item{color() for complex colourspace specifidation. Not handled yet}
#' \item{'transparent' and sometimes 'none' to indicate '#00000000'}
#' \item{'currentColor' or 'currentcolor' to indicate the colour for this element
#'       should be taken from whatever the current 'color' property is}
#' \item{for SVG, colours colours can be references to 'linearGradient' and 'radialGradient'
#'       specifications, or patterns}
#' }
#'
#' Ref: \url{https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#currentcolor_keyword}
#'
#' @param x CSS colour
#'
#' @importFrom grDevices colours
#' @return hex colour (always with alpha channel)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_colour_to_hex <- function(x) {

  x <- tolower(trimws(x))

  if (startsWith(x, "rgb")) {
    parse_colour_rgb_to_hex(x)
  } else if (startsWith(x, "hsl")) {
    parse_colour_hsl_to_hex(x)
  } else if (startsWith(x, "hcl")) {
    warning("hcl() support not done")
    '#000000FF'
  } else if (startsWith(x, "lab")) {
    warning("lab() support not done")
    '#000000FF'
  } else if (startsWith(x, "color")) {
    warning("color() support not done.")
    '#000000FF'
  } else if (startsWith(x, '#')) {
    parse_colour_hex(x)
  } else if (x %in% names(css_colour_lookup)) {
    css_colour_lookup[[x]]
  } else if (x == 'transparent' || x == 'none') {
    '#00000000'
  } else if (x %in% colours()) {
    r_colours[[x]]
  } else if (x == 'currentcolor') {
    warning("currentcolor not handled here")
    '#000000FF'
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Multiply alpha channel by given value
#'
#' @param x hex colour with alpha with total of 8 hex digits
#' @param alpha alpha [0, 1]
#'
#' @importFrom  grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
alpha_mul <- function(x, alpha = 1) {

  stopifnot(startsWith(x, '#') && nchar(x) == 9)
  stopifnot(alpha >= 0 && alpha <= 1)

  v <- grDevices::col2rgb(x, alpha = TRUE)/255

  v[4] <- v[4] * alpha

  grDevices::rgb(v[1], v[2], v[3], v[4])
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set alpha channel
#'
#' @param x hex colour with alpha with total of 8 hex digits
#' @param alpha alpha [0, 1]
#'
#' @importFrom  grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
alpha_set <- function(x, alpha = 1) {

  stopifnot(startsWith(x, '#') && nchar(x) == 9)
  stopifnot(alpha >= 0 && alpha <= 1)

  v <- grDevices::col2rgb(x, alpha = FALSE)/255

  grDevices::rgb(v[1], v[2], v[3], alpha)
}



