% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-css.R
\name{parse_rule}
\alias{parse_rule}
\title{Parse a rule from the stream at the current location}
\usage{
parse_rule(stream)
}
\arguments{
\item{stream}{stream}
}
\value{
name list of list(rule_name = list(property = value, ...))
}
\description{
A rule here is a selector and a list of property/value pairs
}
