#' @title Go fish iter

#' @description \code{go_fish_iter} applies the function \code{go_fish} to the
#' entire fleet with an lapply.

#' @param fleets_params is the parameter settings initialised from \code{_init_fleets}
#' @param fleets_catches is the DF initialised from \code{_init_fleets}
#' @param Pop is the population matrix for all populations
#' @param sp_fleet_catches is a list of spatial catches (as a Numeric matrix) for the fleet of each
#' population

#' @return is a list with the objects catch detailing the fleet catches and
#' catch_matrices detailing the spatial catches, to input to the delay difference
#' model

#' @examples None as yet

#' @export

fish_iters <- function (FUN, sim_init = sim, fleets_params = NULL, fleets_catches = NULL, 
			pops = NULL, t = t,...) {
	out <- lapply(seq(length(fleets_params)), function(x) { res <- go_fish(sim_init = sim,
							   fleet_params = fleets_params[[x]],
							   fleet_catches = fleets_catches[[x]][["catch"]],
							   sp_fleet_catches = fleets_catches[[x]][["catch_matrices"]],
							   t = t, pops = pops)
	       })
}

