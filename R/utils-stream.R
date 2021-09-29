



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A minimal stream object (reminiscent of an R6 class)
#'
#' This tream object (an environment) is used to encapsulate a
#' vector of named tokens, an index of the current position, and some simple
#' tools for advancing the stream, consuming tokens, etc
#'
#' @param tokens named
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init_stream <- function(tokens) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tokens must be fully named
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nn <- names(tokens)
  if (is.null(nn) || anyNA(nn) || any(nn == '')) {
    stop("All tokens must be named")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new environment and putthe tokens and meta-info in it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream        <- new.env()
  stream$tokens <- tokens
  stream$values <- unname(tokens)
  stream$types  <- names(tokens)
  stream$idx    <- 1L
  stream$N      <- length(tokens)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # EOS - Are we at the end-of-stream? TRUE?FALSE?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$eos <- function() {
    stream$idx > stream$N
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Is the current request in bounds of the available tokens? TRUE/FALSE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$within_bounds <- function(n) {
    stopifnot(n > 0)
    stream$idx + n <= stream$N + 1L
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Peek at the next 'n' values in the stream.
  # This does not advance the stream position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$peek <- function(n) {
    stopifnot(stream$within_bounds(n))
    stream$tokens[stream$idx + seq.int(n) - 1L]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Skip ahead 'n' tokens
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$skip <- function(n) {
    stopifnot(stream$within_bounds(n))
    stream$idx <- stream$idx + as.integer(n)
    invisible(stream)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume the next 'n' tokens, advancing the stream position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$consume <- function(n) {
    res <- stream$peek(n)
    stream$skip(n)
    res
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What is the distance to the element before one of the given 'types' occurs
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$distance_before_type <- function(types) {

    idx <- which(stream$types %in% types)
    idx <- idx - 1L
    idx <- idx[idx >= stream$idx]
    idx <- idx - stream$idx + 1L

    idx[1]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new substream, by consuming 'n' elements from the current stream
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$substream <- function(n) {
    init_stream(stream$consume(n))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove tokens of a given type and create a new stream
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$copy_without_type <- function(types) {
    init_stream(stream$tokens[!names(stream$tokens) %in% types])
  }


  class(stream) <- 'stream'
  stream
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' s3 method fr printing a stream
#'
#' @param x stream object
#' @param ... other arguments passed to print()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.stream <- function(x, ...) { #nocov start
  tokens <- x$tokens
  names(tokens) <- paste(seq_along(tokens), names(tokens), sep=":")
  print(tokens, ...)
  cat("Token Stream position: ", x$idx, "/", x$N, "\n")



} #nocov end



if (FALSE) {
  tokens <- c(a= 1, b= 2, b= 3, b= 4, c= 5, c= 6, b= 7,
              d= 8, d= 9, e=10, f=11, g=12, h=13, i=14,
              j=15, j=16, j=17, k=18, l=19, m=20, n=21)
  stream <- init_stream(tokens)

  stream$peek(10)

  stream$distance_before_type('c')
  stream$consume(4)
  stream$idx
  stream$peek(4)
}




