% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_spat_f_pops.R
\name{find_spat_f_pops}
\alias{find_spat_f_pops}
\title{find spatial f pops}
\usage{
find_spat_f_pops(
  FUN = find_spat_f,
  sim_init = sim,
  C = C,
  B = B,
  dem_params = NULL,
  ...
)
}
\arguments{
\item{FUN}{is the \code{find_spat_f} function}

\item{sim_init}{is the simulation settings initialised by \code{init_sim}}

\item{C}{is the spatial catch matrices for all populations}

\item{B}{is the spatial biomass for all populations}

\item{dem_params}{are the demographic parameters for all populations (containing
the natural mortality rate, M.}
}
\description{
\code{find_spat_f_pops} applies the \code{find_spat_f} function
to all the populations, returning the spatial fishing mortality rates for
each of the populations.
}
\examples{
None as yet
}
