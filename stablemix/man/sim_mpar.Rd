% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditional_simulation_low_level.R
\name{sim_mpar}
\alias{sim_mpar}
\title{Simulate GEV marginal parameter}
\usage{
sim_mpar(snew, gvar, gscl, nrep = NULL, betas = NULL, formula = NULL,
  data_new = NULL, log.link = FALSE)
}
\arguments{
\item{snew}{observation locations}

\item{gvar}{GP variance}

\item{gscl}{GP range}

\item{nrep}{number of replicates}

\item{betas}{linear coefficients in, e.g., loc = X beta}

\item{formula}{R formula describing form of covariates in GP mean}

\item{data_new}{data.frame containing covariate values at \code{snew}
locations}

\item{log.link}{(logical) should a log link be used for GEV parameter? This
should be set to TRUE when parameter is constrained to be positive. For
example in the GEV scale case.}
}
\description{
Simulate GEV marginal parameter
}
