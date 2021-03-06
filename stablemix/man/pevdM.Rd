% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evd_functions.R
\name{pevdM}
\alias{pevdM}
\title{R wrapper for C function to calculate GEV cdf}
\usage{
pevdM(q, loc, scale, shape, lower.tail = TRUE, log.p = FALSE)
}
\arguments{
\item{q}{matrix of quantiles at which to evaluate GEV}

\item{loc}{matrix of GEV location parameters}

\item{scale}{matrix of GEV scale parameters}

\item{shape}{matrix of GEV shape parameters}

\item{lower.tail}{(logical) should lower tail be returned?}

\item{log.p}{(logical) should probabilites be log-transformed?}
}
\value{
GEV cumulative probabilities for upper or lower tail
}
\description{
R wrapper for C function to calculate GEV cdf
}
