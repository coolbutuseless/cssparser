% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-css.R
\name{parse_nested_at}
\alias{parse_nested_at}
\title{Parse a 'nested at' rule from the stream at the current location.}
\usage{
parse_nested_at(stream)
}
\arguments{
\item{stream}{stream}
}
\value{
name list of list(rule_name = list(property = value, ...))
}
\description{
Nested 'ats' are things like \code{@media} where there is a nested
stylesheet as part of this declaration.
}
