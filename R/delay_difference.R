#' @title Delay-difference (weekly)
#'
#' @description \code{delay_difference} implements a two-stage delay-difference
#' model with a weekly time-step after \emph{Dichmont 2003}. Given the starting
#' biomass, overall mortality and recruitment it returns the biomass in wk+1.

#' @param K is a Numeric vector describing growth.  Note:
#' K is transformed to rho with \emph{\eqn{\rho = exp{-K}}} for the model.
#' estimate of instantaneous fishing mortality (obtained elsewhere, via
#' \code{\link{find_f}} and \code{\link{baranov_f}} functions.  
#' @param F is the weekly fishing mortality rate.
#' @param M is a Numeric vector of the instantaneous rate of natural mortality for the population
#' @param wt is a Numeric vector of the weight of a fish when fully recruited
#' @param wtm1 is a Numeric vector of the weight of a fish before its recruited
#' @param R is a Numeric vector of the annual recruitment for the population in
#' numbers
#' @param B is the biomass of the population during \emph{\eqn{wk_{t}}}
#' @param Bm1 is a Numeric vector of the biomass of the population in the previous week
#' \emph{\eqn{wk_{t-1}}}
#' @param al is a Numeric vector of the proportion of recruits to the fishery
#' in \emph{\eqn{wk_{t}}}
#' @param alm1 is a Numeric vector of the proportion of recruits to the fishery
#' in \emph{\eqn{wk_{t-1}}}

#' @return Returns the biomass at the beginning of the following week,
#' \emph{\eqn{wk_{t+1}}}

#' @examples
#' delay_diff(K = 0.3, F = 0.2, M = 0.2, wt = 1, wtm1 = 0.1, R = 1e6, B = 1e5,
#' Bm1 = 1e4, al = 0.5, alm1 = 0.1)

#' @export

delay_diff <- function(K = 0.3, F = NULL, M = 0.2, wt = 1, wtm1 = 0.1, R = NULL, B = NULL, Bm1 = NULL, al = NULL, alm1 = NULL) {
	rho <- exp(-K)

	res <- ((1 + rho) * B * exp(-(F + M))) - 
		     (((rho * exp(-(F + M)))) * ((Bm1 * exp(-(F + M))) + (wtm1 * alm1 * R)))  +
		    (wt * al * R)
	return(res)
}


