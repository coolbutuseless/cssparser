
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patterns for parsing an "rgb()" or "rgba()" colour specificaiton
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgb_patterns <- c(
  number      = "[+\\-]?(?:0|[\\.0-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  percent     = "%",
  comma       = ",",
  slash       = "/",
  whitespace  = "\\s+",
  opening     = "rgba|rgb",
  start       = "\\(",
  stop        = "\\)"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse an rgb() or rgba() colour spec to a hex colour
#'
#' MDN \url{https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb()}
#'
#' @param rgb_text string representing an \code{rgb()} or \code{rgba()} colour
#'        e.g. \code{'rgb(255, 0, 0)'}
#'
#' @return hex colour
#'
#' @examples
#' \dontrun{
#' parse_colour_rgb_to_hex('rgb(255, 0, 0)')
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_colour_rgb_to_hex <- function(rgb_text) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tokenisze and remove rubbish
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(rgb_text, rgb_patterns)
  tokens <- tokens[!names(tokens) %in% c('opening', 'start', 'stop', 'whitespace', 'comma', 'slash')]
  tokens <- trimws(tokens)

  has_alpha <- sum(names(tokens) == 'number') == 4
  alpha_is_percentage <- has_alpha && names(tokens)[length(tokens)] == 'percent'

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Handle percentages by rescaling from [0-100%] to 0-255
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  percent_sign_idx <- which(names(tokens) == 'percent')
  percent_idx      <- percent_sign_idx - 1L

  tokens[percent_idx] <- as.numeric(tokens[percent_idx]) * 255.0 / 100

  tokens <- tokens[!names(tokens) == 'percent']

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all to numeric
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- as.numeric(tokens)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert fractional alpha (if present) to range [0, 255]
  #
  # alpha must be fraction in range [0, 1]. So scale to [0, 255]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!has_alpha) {
    tokens <- c(tokens, 255L)
  }

  if (has_alpha && !alpha_is_percentage) {
    tokens[4] <- tokens[4] * 255
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert rgba values to integer, and collapse to a hex string '#rrggbbaa'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- as.integer(round(tokens))

  grDevices::rgb(tokens[1], tokens[2], tokens[3], tokens[4], maxColorValue = 255)
}
