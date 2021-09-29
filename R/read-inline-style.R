
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse CSS declaration block (aka inline style) to a named list of CSS declarations
#'
#' @param inline_style set of ";"-delmited declarations i.e. an inline style,
#'        or a CSS declaration block: e.g.
#'        "stroke:red; fill: black; stroke-width:12"
#'
#' @return named list of declarations (i.e. property/value pairs)
#'         e.g. \code{list(stroke = 'red', fill = 'black', 'stroke-width' = 12)}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_inline_style <- function(inline_style) {

  if (is.null(inline_style) ||
      is.na(inline_style) ||
      is.character(inline_style) && (length(inline_style) == 0 || nchar(inline_style) ==0)) {
    return(NULL)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Short-circuit if the inline_style is not valid text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is_char1(inline_style))

  inline_style <- trimws(inline_style)
  if (startsWith(inline_style, "{")) {
    stopifnot(endsWith(inline_style, "}"))
    inline_style <- substr(inline_style, 2, nchar(inline_style) - 1)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the string into name/value pairs. splitting on ';' and ':'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  elems <- strsplit(trimws(inline_style), ";")[[1]]

  name_value_pairs <- strsplit(elems, ":")
  name_value_pairs <- trimws(unlist(name_value_pairs))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Must be an even number of elements here as each declaration must
  # consistent of a property/value pair
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(name_value_pairs) %% 2 != 0) {
    stop("read_inline_style(): error when parsing inline_style = '", inline_style, "'")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reassemble into a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style_names  <- name_value_pairs[c(TRUE , FALSE)]
  style_values <- name_value_pairs[c(FALSE, TRUE )]

  decls   <- as.list(style_values)
  names(decls) <- style_names

  decls
}


