

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patterns for tokenising a simple style sheet
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_patterns <- c(
  comment    = "/\\*.*?\\*/",
  colon      = ":",  # used to sep prop name/value and in media queries
  semicolon  = ";",
  open       = "\\{",
  close      = "\\}",
  url        = "url\\(.*?\\)",
  whitespace = "\\s+",
  nested_at  = "@media|@supports|@document|@keyframes|@viewport|@font-feature-values|@color-profile",
  simple_at  = "@page|@font-face|@viewport|@counter-style|@property@color-profile",
  at         = "@",
  symbol     = "[^\\s:;]+"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert CSS stylesheet from text or a file into a list object
#'
#' @param stylesheet CSS stylsheet. Either as a character string containing the CSS,
#'        or a path to a file containing CSS text
#'
#' @return named list of lists, where the top-level name is the CSS selector,
#'         and the value is a list of property/value pairs for this selector
#'         (i.s. a CSS declaration block)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_css <- function(stylesheet) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # IF it exists as a file, try and read it.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (file.exists(stylesheet)) {
    stylesheet <- paste(readLines(stylesheet), collapse = "\n")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Short circuit if it doesn't appear to be valid string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is_char1(stylesheet))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is a sledgehammer to remove some comments in firefox-user.css
  # and chrome-user.css.
  # Obviously, the css files in their repos aren't used "as-is", but are
  # run through a pre-processor to strip/evaluate directives
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stylesheet <- strsplit(stylesheet, "\n")[[1]]
  stylesheet <- stylesheet[!grepl("^%"     , stylesheet, perl=TRUE)]
  stylesheet <- paste(stylesheet, collapse = " ")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the CSS text into tokens, and ignore the comments.
  # Whitespace is still needed as selectors can be
  # whitespace sensitive (but property/value declarations aren't)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(trimws(stylesheet), css_patterns)
  tokens <- tokens[!names(tokens) %in% 'comment']
  tokens

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove any leading/trailing whitespace which may have been
  # when emoving comments.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while(names(tokens[1]) == 'whitespace') {
    tokens <- tokens[-1]
  }

  while(names(tokens[length(tokens)]) == 'whitespace') {
    tokens <- tokens[-length(tokens)]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # replace any runs of whitespace with a single whitespace token.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ws <- rle(names(tokens) == 'whitespace')
  ws$values[ws$lengths == 1] <- FALSE
  idx <- inverse.rle(ws)
  idx <- duplicated(idx) & idx
  idx <- which(idx)
  if (length(idx) > 0) {
    tokens <- tokens[-idx]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enforce all whitespaces tokens to refer to just a single space char
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens[names(tokens) == 'whitespace'] <- ' '


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert this simple named character vector into a token stream i.e.
  # a stream = environent containing tokens + current index.
  #
  # We'll pass this stream to helper functions to parse particular components.
  #
  # By making this an environment (and not just a list) we'll be passing
  # this `stream` by reference, which means the called functions can update
  # the index but never need to actually return the environment object
  # to the calling function.
  #
  # This is very much a poor-man's version of an R6 object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream <- init_stream(tokens)

  parse_rules_from_stream(stream)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse rules from the stream at the current location
#'
#' A 'rule' is a 'selector' + a list of property/value pairs.
#'
#' @param stream token stream
#'
#' @return name list of list(rule_name = list(property = value, ...))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_rules_from_stream <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # list of all rules
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules <- list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out all the selectors and property lists
  #
  #    selector  {
  #        name:  value value value ;
  #        name:  value value value ;
  #    }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while(!stream$eos()) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Most of the time, regular CSS rules start with a symbol i.e.
    # the selector.
    #
    # If the rule starts with '@' then it could be a '@media' selector (
    # which contains a nested stylesheet), or '@font-face' or a number of
    # different 'at' directives.).  Besides '@media', the '@' queries are
    # just parsed into a generic object, and nothing else is ever done with them
    #
    # Selectors for pseudo classes/elements start with a colon
    #
    # The following looks at the first token in what should be the 'selector'
    # and then calls the parser relevant to that type
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    while(names(stream$peek(1)) == 'whitespace' ) {
      stream$skip(1)
    }
    cur <- stream$peek(1)
    if (cur == '}') {
      break;
    } else if (names(cur) == 'simple_at') {
      this_rule <- parse_rule(stream)
    } else if (names(cur) == 'nested_at') {
      this_rule <- parse_nested_at(stream)
    } else if (names(cur) == 'at') {
      this_rule <- parse_at(stream)
    } else if (names(cur) == 'symbol') {
      this_rule <- parse_rule(stream)
    } else if (names(cur) == 'colon') {
      this_rule <- parse_rule(stream)  # this is a pseudo-class/element selector
    } else  {
      stop(
        "Unexpected token at pos ", stream$idx,
        ": ", cur, " (", names(cur), ")"
      )
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add it to the list of all rules
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rules <- c(rules, this_rule)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return a named list of rules
  #   * Each name is a selector
  #   * Each top-level list is a list of declarations (i.e. property/value pairs)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rules
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a simple "at' rule from the stream at the current location
#'
#' This is really generic and lumps everything up to the next semi-colon into
#' a single character string.
#'
#' @param stream stream
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_at <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume the '@' and the actual label
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  at  <- paste(stream$consume(2), collapse = "")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The RHS of the '@' rule is everything up to the next semicolon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n   <- stream$distance_before_type("semicolon")
  rhs <- stream$consume(n)
  rhs <- trimws(paste(rhs, collapse = ""))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse into list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  atrule <- list(rhs)
  names(atrule) <- at

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume and discard the final ";"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$consume(1)

  atrule
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a 'nested at' rule from the stream at the current location.
#'
#' Nested 'ats' are things like \code{@media} where there is a nested
#' stylesheet as part of this declaration.
#'
#' @param stream stream
#'
#' @return name list of list(rule_name = list(property = value, ...))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_nested_at <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out 'selectors' and 'rules'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  selectors <- parse_selectors(stream)
  rules     <- parse_rules_from_stream(stream)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Jump over final "}" since rules are nested within
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$skip(1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Nested at
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nested_at <- rep(list(rules), length(selectors))
  names(nested_at) <- selectors

  nested_at
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a rule from the stream at the current location
#'
#' A rule here is a selector and a list of property/value pairs
#'
#' @param stream stream
#'
#' @return name list of list(rule_name = list(property = value, ...))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_rule <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All rules start with a selector - maybe multiple selectors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  selectors <- parse_selectors(stream)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Then a sequence of properties of the form:
  #   "name: value;"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  decls <- list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Find the closeing "}" of the CSS rule
  # - Copy everything up to there into a new stream
  # - remove 'whitespace' as it isn't used in declaration blocks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n <- stream$distance_before_type('close')
  substream <- stream$substream(n)$copy_without_type('whitespace')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse declaratons until we run out of tokens in this substream
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while(!substream$eos()) {
    decl  <- parse_declaration(substream)
    decls <- c(decls, decl)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Jump over final "}" in rule
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$skip(1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replicate the final set of declarations - one copy for each
  # selector name in 'selectors'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rule <- rep(list(decls), length(selectors))
  names(rule) <- selectors

  rule
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract selector from CSS tokens stream
#'
#' The selector is everything from the start of the rule to the first open-brace
#'
#' e.g.  .circle {fill = 'black'} ->  '.circle'
#'
#' @param stream environment containing tokens and the current index. This is
#'        an internal only datastructure to assist in parsing
#'
#' @return single character string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_selectors <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume everything up to the opening bracket of the declaration block
  # All of this must be selectors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n   <- stream$distance_before_type('open')
  res <- stream$consume(n)
  res <- trimws(paste(res, collapse = ""))

  # selector_parts <- c()
  # while(stream$peek(1) != '{') {
  #   selector_parts <- c(selector_parts, stream$consume(1))
  # }
  # res <- paste(selector_parts, collapse = "")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Skip over opening "{"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$skip(1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multpile selectors are separated by ","
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  trimws(strsplit(res, ",")[[1]])
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract a declaration (i.e. a property/value pair) from token stream
#'
#' e.g. "fill: black; color: blue" -> list(fill = 'black', color = 'blue')
#'
#' @inheritParams parse_selectors
#'
#' @return a named list i.e. the name/value property pair
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_declaration <- function(stream) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # property names are always a single word. maybe hyphenated, followed
  # by a colon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  property  <- stream$consume(1) # read theproperty
  stream$skip(1)# jump over the  'colon'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Values can be multiple symbols, so read until the end of this
  # declaration, and lump them all the tokens together:
  # e.g. 'font: 1.2em "Fira Sans", sans-serif;'
  #
  # Note: For the final declaration in a declaration block, the
  #       trailing semi-colon is option (but considered good practice),
  #       so be prepared to deal with it.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  value_parts <- c()
  while(!stream$eos() && !stream$peek(1) %in% c(";", "}")) {
    value_parts <- c(value_parts, stream$consume(1))
  }
  value <- trimws(paste(value_parts, collapse = " "))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If property is marked as 'important', then set the 'important' attribute
  # on this object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (grepl("\\!\\s*important", value)) {
    value <- trimws(gsub("\\!\\s*important", "", value))
    attr(value, "important") <- TRUE
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume the ';' if it exists
  # According to CSS spec, the last declaration doesn't actually have to
  # have the trailing ";"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!stream$eos() && stream$peek(1) == ';') {
    stream$skip(1)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Turn this name/value pair into a named list with 1 element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  decl <- list(value)
  names(decl) <- property

  decl
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A version of cat which has an indentation depth
#'
#' @param depth the indentation depth
#' @param ... arguments passed to cat()
#' @param sep argument passed to cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat_indent <- function(depth, ..., sep = "") { #nocov start
  ind <- paste(rep("  ", depth), collapse = "")

  cat(ind)
  cat(..., sep = sep)
} #nocov end



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pretty Printing of a CSS stylesheet (as produced by \code{read_css()})
#'
#' @param x object representing a CSS stylesheet
#' @param depth the recursion depth of this block (used for indentation)
#' @param ... other arguments ignored
#' @param rulesep separator between multiple rules
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_pretty_print <- function(x, depth = 0, rulesep = "\n", ...) { #nocov start
  for (i in seq_along(x)) {
    selector <- names(x)[[i]]
    cat_indent(depth, selector, " {\n")

    if (startsWith(selector, "@media")) {
      css_pretty_print(x[[i]], depth = depth + 1, rulesep = rulesep, ...)
    } else {
      style_pretty_print(x[[i]], depth = depth + 1)
    }

    cat_indent(depth, "}\n")
    if (i != length(x)) cat_indent(depth, rulesep)
  }
} #nocov end


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pretty Printing of a single style or declaration block
#'
#' @param x object representing a list of property/value declarations
#' @param depth the recursion depth of this block (used for indentation)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_pretty_print <- function(x, depth = 0) { #nocov start
  decl_block <- x
  for (idx in seq_along(decl_block)) {
    val <- decl_block[idx]
    if (isTRUE(attr(val[[1]], 'important'))) {
      cat_indent(depth, names(val), ": ", val[[1]], " !important;\n")
    } else {
      cat_indent(depth, names(val), ": ", val[[1]], ";\n")
    }
  }
} #nocov end





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

  zz <- parse_rules(stylesheet)

  zz
}


