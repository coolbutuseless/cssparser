

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Make a distinct, unlinked, independent copy of an xml node
#'
#' Since XML documents and nodes are almost always handled 'by-reference'
#' if you want to keep an original untouched when adding nodes etc,
#' you need to make an unlinked copy  of it.
#'
#' @param x xml document
#'
#' @return new, unlinked xml document
#'
#' @import xml2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xml_duplicate <- function(x) {
  chr <- as.character(x)
  if (startsWith(chr, "<!DOCTYPE")) {
    xml2::read_html(chr)
  } else if (startsWith(chr, "<?xml")) {
    xml2::read_xml(chr)
  } else {
    stop("Not sure if this is XML or HTML. Hopefully this error never happens.")
  }
}


