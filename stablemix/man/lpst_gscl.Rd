% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_functions.R
\name{lpst_gscl}
\alias{lpst_gscl}
\title{Log-full conditional for log-GP basis variance parameter}
\usage{
lpst_gscl(gscl, s, lK, gvar, rng_max)
}
\arguments{
\item{gscl}{GP scale/range}

\item{s}{observation location matrix (nloc x 2)}

\item{lK}{log-Gaussian process kernels which sum to 1 at each location after
exponentiation. Matrix dimensions: nloc x nbasis}

\item{gvar}{GP variance}

\item{rng_max}{standard deviation for prior}
}
\description{
Log-full conditional for log-GP basis variance parameter
}
