


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lex patterns for CSS selectors
# css class/id -?[_a-zA-Z]+[_a-zA-Z0-9-]*
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
selector_patterns <- c(
  not        = ":not",
  attr       = "\\[.*?\\]",
  id         = '#-?[_a-zA-Z]+[_a-zA-Z0-9-]*',
  pclass     = '-?[_a-zA-Z]+[_a-zA-Z0-9-]*:-?[_a-zA-Z]+[_a-zA-Z0-9-]*',
  pclass2    =                           ':-?[_a-zA-Z]+[_a-zA-Z0-9-]*',
  pelem     = '-?[_a-zA-Z]+[_a-zA-Z0-9-]*::-?[_a-zA-Z]+[_a-zA-Z0-9-]*',
  pelem2    =                           '::-?[_a-zA-Z]+[_a-zA-Z0-9-]*',
  class      = '\\.[^.#\\s]+',
  type       = "\\w+",
  other      = "\\+|~|>|\\(|\\)",
  star       = "\\*",
  whitespace = "\\s+"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate selector specificity
#'
#' Algorithm defined here: \url{https://drafts.csswg.org/selectors-3/#specificity}
#'
#' A selector's specificity is calculated as follows:
#' \itemize{
#'   \item{count the number of ID selectors in the selector (= a)}
#'   \item{count the number of class selectors, attributes selectors, and
#'          pseudo-classes in the selector (= b)}
#'   \item{count the number of type selectors and pseudo-elements in the selector (= c)}
#'   \item{ignore the universal selector}
#' }
#'
#' Selectors inside the negation pseudo-class are counted like any other,
#' but the negation itself does not count as a pseudo-class.
#'
#' Concatenating the three numbers a-b-c (in a number system with a large base)
#' gives the specificity.
#'
#' Since we only really have base-10 numbers (rather than numbers in
#' arbitrarily large bases), just use 2 digits for each of the counts.  Hopefully it
#' is really unlikely that any of the counts exceeds 99!
#'
#'
#' @param sel selector
#'
#' @return numeric value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
selector_specificity <- function(sel) {

  stopifnot(is_char1(sel))
  sel <- trimws(sel)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tokenize the selector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(sel, selector_patterns)
  bits   <- names(tokens)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tally the various parts: ids, classes, pseudo-classes (pclass, pclass2),
  # pseudo-elements (pelem, pelem2), type selectors and attribute selectors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Nclass  <- sum(bits == 'class')
  Nid     <- sum(bits == 'id')
  Npclass <- sum(bits %in% c('pclass', 'pclass2'))
  Npelem  <- sum(bits %in% c('pelem', 'pelem2'))
  Ntype   <- sum(bits == 'type')
  Nattr   <- sum(bits == 'attr')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # calculate the numbers a, b, c
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  a <- Nid
  b <- Nclass + Nattr + Npclass
  c <- Ntype + Npelem


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Give them 2 digits each in the output.
  # This is to deal with the situation (maybe) where b = 11.  We do NOT want
  # that value overflowing into the 'b' value.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  as.integer(
    sprintf("%02i%02i%02i", a, b, c)
  )
}










