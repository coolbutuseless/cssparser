% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-css.R
\name{parse_selectors}
\alias{parse_selectors}
\title{Extract selector from CSS tokens stream}
\usage{
parse_selectors(stream)
}
\arguments{
\item{stream}{environment containing tokens and the current index. This is
an internal only datastructure to assist in parsing}
}
\value{
single character string
}
\description{
The selector is everything from the start of the rule to the first open-brace
}
\details{
e.g.  .circle {fill = 'black'} ->  '.circle'
}
