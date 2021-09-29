

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file is all about splitting up the "font" shortand property into
# explicity property/value pairs
#
# I.e. rather than inherit the 'font' property, split it into "font-style",
# "font-family" etc, and then inherit those properties.
#
# NOte that when the shorthand 'font' property is used, any vlue that is not
# explicity defined is set to its default value.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ref: https://developer.mozilla.org/en-US/docs/Web/CSS/font
#
# If font is specified as a shorthand for several font-related properties, then:
#
#  it must include values for:
#     <font-size>
#     <font-family>
#
#  it may optionally include values for:
#     <font-style>
#     <font-variant>
#     <font-weight>
#     <font-stretch>
#     <line-height>
#
# - font-style, font-variant and font-weight must precede font-size
# - font-variant may only specify the values defined in CSS 2.1, that is normal and small-caps
# - font-stretch may only be a single keyword value.
# - line-height must immediately follow font-size, preceded by "/", like this: "16px/3"
# - font-family must be the last value specified.
#
#        font-style / font-variant / font-weight
#    (*) font-size or font-size/font-height
#    (*) font-family
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_variant_defaults <- list(
  `font-variant-alternates` = "normal",
  `font-variant-caps`       = "normal",
  `font-variant-east-asian` = "normal",
  `font-variant-ligatures`  = "normal",
  `font-variant-numeric`    = "normal"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_defaults <- list(
  `font-family`  = 'sans-serif', # familyname, serfi, sans-serif, monospace, cursive, ...
  `font-size`    = 'medium', # xx-/xx- small, medium, x-,xx-,xxx-large, larger, smaller, length, percentage
  `font-stretch` = 'normal', # normal, semi-/extra-ultra condensed/expanded, percentage
  `font-style`   = 'normal', # normal, italic, oblique, oblique<angle>
  `font-variant` = 'normal', # short hand for many font-variant-*
  `font-weight`  = 'normal', # normal, bold, lighter, bolder, number[1,100]
  `line-height`  = 'normal', # normal, number, length, percentage

  # Not settable via '@font' shorthand, but values reset when this is used
  `font-size-adjust` = 'none',
  `font-kerning`     = 'auto'   # auto, normal, none
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# font-stretch may only be a single keyword value.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_stretch_keywords <- c(
  'ultra-condensed', 'extra-condensed', 'condensed', 'semi-condensed',
  'normal',
  'semi-expanded', 'expanded', 'extra-expanded', 'ultra-expanded'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_variant_keywords <- c(
  'small-caps'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_style_keywords <- c(
  'italic', 'oblique', 'oblique \\d+deg'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_weight_keywords <- c(
    # normal
    'bold', 'lighter', 'bolder', seq(100, 900, 100)
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_size_keywords <- c(
  'xx-small', 'x-small', 'small',
  'medium',
  'large', 'x-large', 'xx-large', 'xxx-large',
  'smaller', 'larger'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_size_numeric <- "[\\d\\.]+[^\\d\\s/;]+"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_system_keywords <- c(
  'caption',
  'icon',
  'menu',
  '>message-box',
  'small-caption',
  'status-bar'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_patterns <- c(
  system_font   = paste(font_system_keywords , collapse = "|"),
  stretch       = paste(font_stretch_keywords, collapse = "|"),
  style         = paste(font_style_keywords  , collapse = "|"),
  variant       = paste(font_variant_keywords, collapse = "|"),
  weight        = paste(font_weight_keywords , collapse = "|"),
  lineheight    = "/([^\\s]+)",
  size          = paste(c(font_size_numeric, font_size_keywords), collapse = "|"),
  family_dquote = '"(.*?)"',
  family_squote = "'(.*?)'",
  family_raw    = "[^;,\\s/]+",
  comma         = ",",
  semicolon     = ";",
  whitespace    = "\\s+"
)

if (FALSE) {
  tokens <- lex("italic small-caps bold 16px/2 cursive;", font_patterns)
  tokens[!names(tokens) %in% c('whitespace', 'comma', 'semicolon')]
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Split font shorthand property into a named list of verbose "font-*" properties
#'
#' This is not perfect/finished but it is a step in the right direction.
#'
#' For example: this function does not split the "font-variant" shorthand
#'
#' @param font_text the text of the "font" declaration e.g.
#'        "italic 1.2em "Fira Sans", serif;"
#'
#' @return list of font-related property/value declarations e.g.
#'         \code{list('font-family'='serif', font-size='1.2em', ...)}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
split_font_shorthand <- function(font_text) {
  tokens <- lex(font_text, font_patterns)
  tokens <- tokens[!names(tokens) %in% c('whitespace', 'comma', 'semicolon')]

  font <- font_defaults
  font[['font-family']] <- NULL

  for (i in seq_along(tokens)) {
    this_value <- tokens[[i]]
    this_type  <- names(tokens)[i]

    switch(
      this_type,
      stretch       = { font[['font-stretch']] <- this_value },
      style         = { font[['font-style'  ]] <- this_value },
      variant       = { font[['font-variant']] <- this_value },
      weight        = { font[['font-weight' ]] <- this_value },
      lineheight    = { font[['line-height' ]] <- this_value },
      size          = { font[['font-size'   ]] <- this_value },
      family_raw    =,
      family_dquote =,
      family_squote = { font[['font-family']] <- c(font[['font-family']], this_value) }
    )

  }

  font[['font-family']] <- font[['font-family']]  %||% font_defaults[['font-family']]

  font
}







