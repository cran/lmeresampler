% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{extract_parameters}
\alias{extract_parameters}
\title{Extract parameters from LME models}
\usage{
extract_parameters(model)
}
\arguments{
\item{model}{the fitted model object, either an \code{merMod} or \code{lme} object.}
}
\value{
A named vector of parameters.
}
\description{
A utility function that extracts the fixed effects and variance component
estimates from a fitted \code{merMod} or \code{lme} object.
}
