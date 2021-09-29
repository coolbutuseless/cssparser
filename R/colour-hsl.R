
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patterns for parsing an "rgb()" or "rgba()" colour specificaiton
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hsl_patterns <- c(
  number      = "[+\\-]?(?:0|[\\.0-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  percent     = "%",
  comma       = ",",
  slash       = "/",
  whitespace  = "\\s+",
  opening     = "hsla|hsl",
  start       = "\\(",
  stop        = "\\)"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a hsl/hsla colour spec to a hex colour
#'
#' MDN \url{https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/hsl()}
#'
#' @param colour_func_text string representing an \code{hsl()} or \code{hsla()} colour
#'        e.g. \code{'hsl(50, 0.5, 0.5)'}
#'
#' @return hex colour
#'
#' @examples
#' \dontrun{
#' parse_colour_hsl_to_hex('hsl(50, 0.5, 0.5)')
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_colour_hsl_to_hex <- function(colour_func_text) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tokenisze and remove rubbish
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(colour_func_text, hsl_patterns)
  tokens <- tokens[!names(tokens) %in% c('opening', 'start', 'stop', 'whitespace', 'comma', 'slash')]
  tokens <- trimws(tokens)


  has_alpha <- sum(names(tokens) == 'number') == 4
  alpha_is_percentage <- has_alpha && names(tokens)[length(tokens)] == 'percent'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Handle percentages by rescaling from [0-100%] to 0-255
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  percent_sign_idx    <- which(names(tokens) == 'percent')
  percent_idx         <- percent_sign_idx - 1L
  tokens[percent_idx] <- as.numeric(tokens[percent_idx]) / 100
  tokens              <- tokens[!names(tokens) == 'percent']
  tokens              <- as.numeric(tokens)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert fractional alpha (if present) to range [0, 255]
  #
  # alpha must be fraction in range [0, 1]. So scale to [0, 255]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!has_alpha) {
    tokens <- c(tokens, 1)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert HSL to HSV.
  # https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_HSV
  # ToDo: just convert HSL direct to RGB
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  h <- tokens[1] / 360
  v <- tokens[3] + tokens[2] * min(tokens[3], 1 - tokens[3])
  s <- ifelse(v == 0, 0, 2 * (1 - tokens[3]/v))
  a <- tokens[4]

  grDevices::hsv(h, s, v, a)
}






