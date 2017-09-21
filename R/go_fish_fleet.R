#' @title Go fish fleet 

#' @description \code{go_fish_fleet} applies the function \code{go_fish} to the
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

go_fish_fleet <- function (FUN = go_fish, sim_init = sim, fleets_params = NULL, fleets_catches = NULL, 
			sp_fleets_catches = NULL, pops = NULL, t = t,...) {
	out <- lapply(seq(length(fleets_catches)), function(x) { 
			      res <- go_fish(sim_init = sim_init, fleet_params = fleets_params,
					     fleet_catches = fleets_catches[[x]],
					     sp_fleet_catches = sp_fleets_catches[[x]],
					     t = t, pops = pops)
	       })

	# We want the fleet catches together, and the spatial catches together
	out_catches <- lapply(out, '[[', 1)
	out_sp_catches <- lapply(out, '[[', 2)
	out <- list(fleets_catches = out_catches, sp_fleets_catches = out_sp_catches)
	return(out)

}

