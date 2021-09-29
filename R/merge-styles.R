
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://dev.to/muyunyun/inherited-and-non-inherited-in-css-mi4
#
# Not all properties take part in the style cascade.
# e.g. the 'width' set on a parent item isn't inhertied by the child item
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
non_inherited_properties <- c(
  "float",

  "position",
  "left",
  "right",
  "top",
  "bottom",

  "z-index",
  "display",

  "width",
  "max-width",
  "min-width",

  "height",
  "max-height",
  "min-height",

  "margin",
  "margin-bottom",
  "margin-left",
  "margin-right",
  "margin-top",

  "padding",
  "padding-bottom",
  "padding-left",
  "padding-right",
  "padding-top",

  "border",

  "border-color",
  "border-bottom-color",
  "border-left-color",
  "border-right-color",
  "border-top-color",

  "border-style",
  "border-bottom-style",
  "border-left-style",
  "border-right-style",
  "border-top-style",

  "border-width",
  "border-bottom-width",
  "border-left-width",
  "border-right-width",
  "border-top-width",

  "background",
  "background-size",
  "background-image",
  "background-clip",
  "background-color",
  "background-origin",
  "background-position",
  "background-repeat",

  "overflow",
  "overflow-x",
  "overflow-y",

  "text-overflow",
  "vertical-align"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Properties which accumulate values rather than override prior values.
#
# The only one that I've seen in practice is SVG's \code{transform}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
accumulative_properties <- c(
  'transform'  # SVG
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A CSS-aware version of nested \code{modifyList()}
#'
#' If an individual element in the base list has the 'important' attribute
#' set to TRUE, then it does *not* get overwritten.
#'
#' Some properties are accumulative, rather than replacement-based.  E.g.
#' SVGs \code{transform} attribute combines parent + child transform
#' matrices.  For now, any accumulative properties are concatenated into
#' a vector of character stirngs to be dealt with by the user after
#' parsing.
#'
#' Otherwise, this function behaves very much like \code{utils::modifyList()}
#'
#' @param base the original list
#' @param new the new list from which to take values and put into base
#'
#' @return updated version of base list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_merge_core <- function (base, new) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Explode the 'font' shorthand into multiple property/value pairs if
  # it looks like the 'new' list contains an individual 'font-' property.
  # If there isn't a 'font-*' property on 'new', then just keep the compact
  # 'font' shorthand inherited to 'new'.
  # If new contains its own 'font' shorthand property, and 'base' contains
  # 'font-*' properties, then explode the shorthand in new before
  # proceeding.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(names(new)) && any(startsWith(names(new), "font-"))) {
    if ('font' %in% names(base)) {
      exploded_font_shorthand <- split_font_shorthand(base[['font']])
      base[['font']] <- NULL
      base <- c(base, exploded_font_shorthand)
    }
  }
  if ('font' %in% names(new)) {
    if (!is.null(names(base)) && any(startsWith(names(base), "font-"))) {
      exploded_font_shorthand <- split_font_shorthand(new[['font']])
      new[['font']] <- NULL
      new <- c(new, exploded_font_shorthand)
    }
  }



  for (nm in names(new)) {
    if (nm %in% accumulative_properties) {
      # Concatenate accumulative properties like SVG's 'transform'
      base[[nm]] <- c(base[[nm]], new[[nm]])
    } else if (is.list(base[[nm]]) && is.list(new[[nm]])) {
      base[[nm]] <- css_merge_core(base[[nm]], new[[nm]])
    } else {
      if (isTRUE(attr(base[[nm]], 'important'))) {
        if (isTRUE(attr(new[[nm]], 'important'))) {
          base[[nm]] <- new[[nm]]
        } else {
          # base is more important and so stays 'as-is'
        }
      } else {
        base[[nm]] <- new[[nm]]
      }
    }
  }
  base
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Merge multiple stylesheets given in priority order
#'
#' @param ... multiple
#'        CSS objects. The order in which these arguments are given reflect the
#'        priority of the stylesheets from lowest to highest priority.  Later stylesheets
#'        (i.e. high priority) will override any styles declared earlier
#'        (lower priority)
#'
#' @examples
#' \dontrun{
#' css1 <- read_css("chrome_builtin.css")
#' css2 <- read_css("this_page.css")
#' css_merge(css1, css2)
#' }
#'
#' @return final cascaded stylesheet
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_merge <- function(...) {
  ll <- list(...)
  ll <- Filter(Negate(is.null), ll)
  Reduce(css_merge_core, ll)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Merge multiple styles for a given element which are given in priority order
#'
#' @param ... multiple style objects (i.e. declaration blocks).
#'        The order in which these arguments are given reflect the
#'        priority of the styles from lowest to highest priority.  Later styles
#'        (i.e. high priority) will override any styles declared earlier
#'        (lower priority)
#'
#' @examples
#' \dontrun{
#' style1 <- list(color = 'blue', `font-weight` = 'bold')
#' style2 <- list(color = 'red', `font-size`="12px")
#' style_merge(style1, style2)
#' }
#'
#'
#' @return final cascaded result
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_merge <- css_merge

