


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Trim the end off a string
#'
#' @param x string
#' @param len number of characters to trim
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trim <- function(x, len) {
  substr(x, 1, max(nchar(x) - len, 1))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a new CSS length object
#'
#' @param x numeric value
#' @param unit character string for unit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_length <- function(x, unit) {
  attr(x, 'unit') <- unit
  x
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a string value into a numeric value with a 'unit' attribute (if a unit is present)
#'
#' \url{https://www.w3schools.com/CSSref/css_units.asp}
#'
#' @param x a value from CSS.
#'
#' @return \code{css_length} object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_string_as_css_length <- function(x) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Short circuit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is_char1(x)) {
    warning("css_value_as_numeric: Not a number: ", x)
    return(0)
  }

  x <- trimws(x)

  if (x == 'none' || x == '') {
    return(0)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find non-digit suffix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  suffix <- regmatches(x, regexpr("([^\\d]+)$", x, perl = TRUE))
  suffix <- tolower(suffix)

  if (is.null(suffix) || is.na(suffix) || length(suffix) == 0 || nchar(suffix)== 0) {
    return(css_length(as.numeric(x), 'none'))
  }

  x <- trim(x, nchar(suffix))
  x <- as.numeric(x)

  css_length(x, suffix)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a CSS unit into a rough pixel measurement
#'
#' This is a naive conversion.
#'
#' For a proper conversion, it would have to take into account the font-size
#' on the root element (for \code{rem} units), and various things to do with
#' framepoint and viewing size of the element and its parents.
#'
#' @param x object of type \code{css_length} as returned by \code{css_string_to_css_length()}
#' @param font_size font size to calculate 'em' with. default: 12
#' @param ... other arguments ignored
#'
#' @return simple numeric value
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_length_as_pixels <- function(x, font_size = 12, ...) {

  unit  <- attr(x, 'unit', exact = TRUE)
  value <- c(x)

  if (length(unit) == 0) {
    return(value)
  }

  switch(
    unit,
    none  =,
    `%`   =,
    `px`  =,
    `pt`  =,
    `pc`  = value,
    `in`  = value * 96,
    `cm`  = value * 96/2.54,
    `mm`  = value * 96/2.54/10,
    `em`  = value * font_size,
    `ex`  = value * font_size,  # Not correct, but better than nothing
    `ch`  = value * font_size,  # Not correct, but better than nothing
    `rem` = value * font_size,  # Not correct, but better than nothing
    {
      warning("css_value_as_numeric: unknown suffix: ", unit)
      value
    }
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Naively convert a CSS string value (e.g. \code{"1em"}) into the number of pixels
#' this represents.
#'
#' This function is a small wrapper around \code{css_string_as_length()} and
#' \code{css_length_as_pixels()}.
#'
#' This function does some naive conversions, and assumes the display is
#' 96dpi.
#'
#' For more control on the final result, the user is encouraged to use
#' \code{css_string_as_length()} and hand-roll their own unit conversion to
#' their display units.  This can be tricky as some units rely on rendering
#' viewport sizes and font-size on the root node - thus i'll leave that for
#' the user to handle.
#'
#' @param x Character string of a CSS value e.g. "12", "12px", "3em", "47\%"
#' @param percentage_as_fraction Default: TRUE means that if a value is given as
#'        "50%", then the returned numeric value is "0.5".  If this argument is
#'        \code{FALSE}, then a numeric value of 50 would be returned.
#' @param ... other arguments passed to \code{css_length_as_pixels()}
#'
#' @return a numeric value for this length in pixels as best we can with limited knowledge
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_string_as_pixels <- function(x, percentage_as_fraction = TRUE, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the value is already a numeric value (because of some earlier pre-processing
  # then just return it as-is)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(x) || is.na(x)) return(0)
  if (is.numeric(x)) return(x)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert to a css_length value + unit combination
  # Then convert this to pixels. Roughly. Naively.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  len <- css_string_as_css_length(x)
  res <- css_length_as_pixels(len, ...)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Most of the time we'll want a perscentage as a frction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (percentage_as_fraction && attr(len, 'unit') == '%') {
    res <- res/100
  }

  res
}




