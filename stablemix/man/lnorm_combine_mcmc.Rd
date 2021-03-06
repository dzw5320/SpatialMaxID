% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_munging.R
\name{lnorm_combine_mcmc}
\alias{lnorm_combine_mcmc}
\title{Combine MCMC samples from log-Gaussian process basis model}
\usage{
lnorm_combine_mcmc(file.dir, mar_gp_which = c("mu", "sigma"),
  burnin = 100)
}
\arguments{
\item{file.dir}{Directory containing all MCMC datasets (and nothing else)}

\item{mar_gp_which}{vector of strings containing some subset of mu, sigma, xi
indicating which GEV parameters were modeled as Gaussian processes}

\item{burnin}{How many mcmc iterations should be discarded as burnin, if any?}
}
\value{
combined mcmc output list of same format as in stored R files
}
\description{
Since cluster jobs sometimes have max wall times,
it may be necessary to break MCMC run into smaller jobs
of a few days at a time. This function chains together the output from a
collection of smaller jobs that were run sequentially.
}
