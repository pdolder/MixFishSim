#' @title Go fish iter

#' @description \code{go_fish_iter} applies the function \code{go_fish} to the
#' entire fleet an lapply.

#' @param fleets_params is the parameter settings initialised from \code{_init_fleets}
#' @param fleets_catches is the DF initialised from \code{_init_fleets}
#' @param Pop is the population matrix for all populations
#'

#' @return is a list with the objects catch detailing the fleet catches and
#' catch_matrix detailing the spatial catches, to input to the delay difference
#' model

#' @examples None as yet

#' @export

fish_iters <- function (FUN, N = 1, fleets_params = NULL, fleets_catches = NULL, Pop = NULL, t = t,...) {
	out <- lapply(seq(N), function(x) { res <- go_fish(fleet_params = fleets_params[[x]],
							   fleet_catches = fleets_catches[[x]], 
							   t = t, Pop = NULL)
	       })
}

