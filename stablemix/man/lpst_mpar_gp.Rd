% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_functions.R
\name{lpst_mpar_gp}
\alias{lpst_mpar_gp}
\title{Log-full conditional for marginal GEV parameters with GP priors}
\usage{
lpst_mpar_gp(gp, mpar_gvar, mpar_gscl, s, lz, y, lA, lK, alpha, theta, mu,
  sigma, xi, rng_max = NULL)
}
\arguments{
\item{gp}{GEV parameter Gaussian process matrix}

\item{mpar_gvar}{variance parameter for marginal GEV GP}

\item{mpar_gscl}{scale/range parameter for marginal GEV GP}

\item{s}{nloc x 2 matrix of observation location coordinates}

\item{lz}{(n-rep by nloc) matrix of log-transformed stable-mixture
observations}

\item{y}{(n-rep by nloc) matrix of stable-mixture observations on GEV scale}

\item{lA}{(n by number of obs_coord) matrix of 1/alpha-norm scaled
kernel functions at each observation location.}

\item{lK}{(nloc by nbasis) matrix of log-transformed sum constrained kernel
functions}

\item{alpha}{stable alpha parameter: 0 < \code{alpha} < 1. Controls the
tradeoff between all signal (alpha = 0) and all nugget (alpha = 1).}

\item{theta}{stable theta parameter (exponential tilting parameter). This
parameter controls the tail dependence class. \code{theta} = 0 gives
asymptotic dependence, and \code{theta} > 0 gives asymptotic independence.}

\item{mu}{GEV location matrix}

\item{sigma}{GEV scale matrix}

\item{xi}{GEV shape matrix}

\item{rng_max}{optionally define marginal parameter priors based on range of
observation coordinates.}
}
\description{
Log-full conditional for marginal GEV parameters with GP priors
}
