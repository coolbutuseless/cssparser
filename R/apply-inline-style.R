


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Flatten a style to an inline string
#'
#' @param style a named list of property/value pairs
#'
#' @return single string suitable for a \code{style} attribute on an element
#'
#' @examples
#' \dontrun{
#' style_flatten_to_line(list(color='black', border='1px')) # -> "color:black; border: 1px;"
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_flatten_to_inline <- function(style) {
  res <- paste(names(style), unname(style), sep=":", collapse = "; ")

  if (res !='' && !endsWith(res, ';')) {
    res <- paste0(res, ";")
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Apply the CSS to the given HTML, storing the result as inline 'style' tags on each element
#'
#' @param xml html/xml document (as read by \code{xml2::read_xml()})
#' @param css CSS stylesheet to apply  (as read by \code{cssparser::read_css()})
#'
#' @return new xml document with final computed CSS written to inline style
#'         attribute on each element.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_apply_inline <- function(xml, css) {

  xml <- xml_duplicate(xml) # see utils-xml.R

  xpstyle <- css_apply(xml, css)

  for (xpath in names(xpstyle)) {
    node <- xml2::xml_find_first(xml, xpath)
    inline <- style_flatten_to_inline(xpstyle[[xpath]])
    if (is.null(inline) || is.na(inline) || inline=='') {
      xml2::xml_set_attr(node, 'style', NULL)
    } else {
      xml2::xml_set_attr(node, 'style', inline)
    }
  }

  xml
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing zone
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  stylesheet  <- ".greg {
    fill: red;
    color: blue;
  }

  #mary, #jo, #ann:hover {
    fill: white 1px;
    color:   purple;
  }"

  css <- read_css(stylesheet)

  css


  xml <- xml2::read_xml('
  <html>
    <p id="mary"> hello </p>
  </html>
')

  xml <- css_apply_inline(xml, css)

}
