

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Presentation
#
# SVG can define properties for a node in the presentation attributes on that
# node - separate from both the inline style and the inherited style from the
# parent.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_presentation_attrs <- c(
  "alignment-baseline",
  "baseline-shift",
  "clip",
  "clip-path",
  "clip-rule",
  "color",
  "color-interpolation",
  "color-interpolation-filters",
  "color-profile",
  "color-rendering",
  "cursor",
  "direction",
  "display",
  "dominant-baseline",
  "enable-background",
  "fill",
  "fill-opacity",
  "fill-rule",
  "filter",
  "flood-color",
  "flood-opacity",
  "font-family",
  "font-size",
  "font-size-adjust",
  "font-stretch",
  "font-style",
  "font-variant",
  "font-weight",
  "glyph-orientation-horizontal",
  "glyph-orientation-vertical",
  "image-rendering",
  "kerning",
  "letter-spacing",
  "lighting-color",
  "marker-end",
  "marker-mid",
  "marker-start",
  "mask",
  "opacity",
  "overflow",
  "pointer-events",
  "shape-rendering",
  "solid-color",
  "solid-opacity",
  "stop-color",
  "stop-opacity",
  "stroke",
  "stroke-dasharray",
  "stroke-dashoffset",
  "stroke-linecap",
  "stroke-linejoin",
  "stroke-miterlimit",
  "stroke-opacity",
  "stroke-width",
  "text-anchor",
  "text-decoration",
  "text-rendering",
  "transform",
  "unicode-bidi",
  "vector-effect",
  "visibility",
  "word-spacing",
  "writing-mode"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.w3.org/TR/2016/CR-SVG2-20160915/styling.html#PresentationAttributes
#
# SVG has some presentation attributes that apply to the tag they are currently
# on, but otherwise do not take part in the cascade.
#
# For example: a circle can have a 'cx' presentation attribute, but that
# does not flow to child elements (not that child elements of a <circle>
# even make sense).
#
# From the other direction, if a <g> element contains a 'cx' attribute, and
# since 'cx' does not apply to <g>, then this attribute will
#   (a) NOT be part of the <g> element's style
#   (b) NOT cascade to any child elements.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_tag_specific_presentation_attrs <- list(
  circle        = c('cx', 'cy', 'r'),
  ellipse       = c('cx', 'cy', 'rx', 'ry'),
  foreignObject = c('height', 'width', 'x', 'y'),
  image         = c('height', 'width', 'x', 'y'),
  rect          = c('height', 'width', 'x', 'y', 'rx', 'ry'),
  svg           = c('height', 'width', 'x', 'y'),
  symbol        = c('height', 'width', 'x', 'y'),
  use           = c('height', 'width', 'x', 'y'),
  path          = c('d')
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Depth-first recursion into the HTML document node tree to accumulate/cascade styles
#'
#' @param node current XML node
#' @param xpath_styles list containing the accumulated styles for each xpath
#' @param parent_style the style from the direct parent
#' @param svg include SVG presentation tags in the cascade? default: FALSE
#'
#' @return updated xpath style lookup list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
recurse_node_tree <- function(node, xpath_styles, parent_style = list(), svg) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the current style for this node from the xpath style lookup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tag         <- xml2::xml_name(node)
  xpath       <- xml2::xml_path(node)
  xpath_style <- xpath_styles[[xpath]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is SVG, then look at the attributes, and extract any
  # attributes to take part in the cascade.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  presentation_attrs <- list()
  if (svg) {
    attrs      <- xml2::xml_attrs(node)
    attr_names <- c(svg_presentation_attrs, svg_tag_specific_presentation_attrs[[tag]])
    attr_names <- intersect(names(attrs), attr_names)
    presentation_attrs <- as.list(attrs[attr_names])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all the style elements we have access to.
  # The style of the parent is given lowest priority.  The inline style
  # on this element itself has the highest priority/specificity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_style <- css_merge(
    parent_style,
    presentation_attrs,
    xpath_style,
    read_inline_style(
      xml2::xml_attr(node, 'style')
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set the style for this node
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpath_styles[[xpath]] <- this_style

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Expclicity drop any properties that are not inherited before going on
  # to parse the child nodes of the node.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_style[non_inherited_properties] <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tag specific presentation attributes don't get inherited.
  # No reference for this, but I created a few <svg> docs and tested this
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (svg) {
    tag_pres_attrs <- svg_tag_specific_presentation_attrs[[tag]]
    this_style[tag_pres_attrs] <- NULL
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Recurse into child nodes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (child in xml2::xml_children(node)) {
    xpath_styles <- recurse_node_tree(child, xpath_styles, this_style, svg = svg)
  }

  xpath_styles
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a named list (indexed by \code{xpath}) of the final computed style for each element
#'
#' @param xml xml document as returned by \code{xml2::read_html()} or
#'        \code{xml2::read_xml()}
#' @param css list of rules parsed from CSS stylesheet. E.g as returned
#'        by \code{cssparser::read_css()}
#' @param svg include SVG presentation tags in the cascade? default: FALSE
#'
#' @return named list of styles where the name is the \code{xpath} to a
#'         node in the document, and the value is a named list of
#'         property/values for this element.
#'
#' @import xml2
#' @import selectr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_apply <- function(xml, css = list(), svg = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The 'xpath' searching seems to work better if namespaces (ns) are
  # stripped from the xml
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xml <- xml2::xml_ns_strip(xml)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # remove '@' rules e.g. "@media" as these do not take part in
  # the style cascade.
  # Pseudo-elements and classes not handled either, so remove
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  selectors <- names(css) %||% character(0)
  exclude   <- startsWith(selectors, '@') | grepl(':', selectors)
  css       <- css[!exclude]
  selectors <- names(css) %||% character(0)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate CSS selector specificity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  specificity <- vapply(selectors, function(sel) {
    selector_specificity(sel)
  }, integer(1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rearrange the rules in increasing order, so we will end up addressing
  # least specific rules first, and then override with rules of increasing
  # specificity
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  css <- css[order(specificity)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an empty styles list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xpath_styles <- list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each rule
  #   - get the CSS selector
  #   - convert selector to xpath
  #   - fina all elements matching that xpath
  #   - merge the current style in `xpath_styles` with this new rule
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(css)) {
    selector <- names(css)[i]
    rule     <- css[[i]]

    xpath <- selectr::css_to_xpath(selector)
    nodes <- xml2::xml_find_all(xml, xpath)
    for (node in nodes) {
      sig <- xml2::xml_path(node)
      # cat(selector, sig, "\n")
      xpath_styles[[sig]] <- style_merge(
        xpath_styles[[sig]],
        rule
      )
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - In the above, all the direct rules from CSS stylesheet were applied
  # - Now performe the CSS cascase from parent-to-child
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  recurse_node_tree(xml, xpath_styles, svg = svg)
}



if (FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Style sheet
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stylesheet <- '
body {
   color: blue;
}
.light {
  font-weight: lighter;
}
.light.speed {
  font-weight: normal;
}
div > p {
  color: hotpink;
}
'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HTML document
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
html <- xml2::read_html('
<body>
  <html>
  <h1> hello </h1>
  <p> Nothing happens in this p </p>
  <div id="main" style="color:white; width:50%">
    <div id="sidebar">Sidebar</div>
    <div id="content">Main content</div>
    <p> This paragraph </p>
    <ul></ul>
  </div>
  <div id="footer" class="light" style="color:green;"> goodbye </div>
  </html>
</body>
')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the CSS rules and get the root node of the HTML
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules     <- read_css(stylesheet)


  user_agent_css <- list(svg = list(
    `color`             = 'black',
    `clip-path`         = 'none',
    `clip-rule`         = 'nonzero',
    `clipPathUnits`     = 'userSpaceOnUse',
    `fill`              = 'black',
    `fill-opacity`      = 1,
    `fill-rule`         = 'nonzero',
    `font-family`       = 'serif',
    `font-size`         = 12,
    `font-style`        = 'normal',
    `font-weight`       = 'normal',
    `mask`              = 'none',
    `stroke`            = 'none',
    `stroke-dasharray`  = 'none',
    `stroke-dashoffset` = 0,
    `stroke-linecap`    = 'butt',
    `stroke-linejoin`   = 'miter',
    `stroke-miterlimit` = 4,
    `stroke-opacity`    = 1,
    `stroke-width`      = 1,
    `visibility`        = 'visible'
  ))

  svg <- TRUE
  rules <- list()
  user_agent_css <- list()
  parent_style <- list()

  svg_file = 'working/tiger.svg'
  svg_file = 'working/Freesample.svg'
  svg_file = "working/test-transform-translate.svg"
  svg_file = "working/polygon2.svg"
  html <- xml2::read_xml(svg_file) %>% xml2::xml_ns_strip()

  cat(readLines(svg_file), sep = "\n")

  xpstyle <- css_apply(html, user_agent_css = user_agent_css, svg = TRUE)
  xpstyle[['/svg/rect']]
  xpstyle[['/svg/rect']]$width


}








