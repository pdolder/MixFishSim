% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/go_fish_fleet.R
\name{go_fish_fleet}
\alias{go_fish_fleet}
\title{Go fish fleet}
\usage{
go_fish_fleet(
  FUN = go_fish,
  sim_init = NULL,
  fleets_params = NULL,
  fleets_catches = NULL,
  sp_fleets_catches = NULL,
  pops = NULL,
  closed_areas = NULL,
  t = t,
  ...
)
}
\arguments{
\item{fleets_params}{is the parameter settings initialised from \code{_init_fleets}}

\item{fleets_catches}{is the DF initialised from \code{_init_fleets}}

\item{closed_areas}{is a dataframe with the x,y coordinates are any closed}

\item{Pop}{is the population matrix for all populations}

\item{sp_fleet_catches}{is a list of spatial catches (as a Numeric matrix) for the fleet of each population}
}
\value{
is a list with the objects catch detailing the fleet catches and
catch_matrices detailing the spatial catches, to input to the delay difference
model
}
\description{
\code{go_fish_fleet} applies the function \code{go_fish} to the
entire fleet with an lapply.
}
\examples{
None as yet
}
