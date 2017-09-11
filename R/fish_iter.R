#' @title Go fish iter

#' @description \code{go_fish_iter} applies the function \code{go_fish} to the
#' entire fleet an lapply.

#' @param fleets is the parameter settings and catch recording dataframe for all fleets
#' @param Pop is the population matrix for all populations
#'

#' @return is a list with the objects catch detailing the fleet catches and
#' catch_matrix detailing the spatial catches, to input to the delay difference
#' model

#' @examples None as yet

#' @export

fish_iters <- function (FUN, N = 1, fleets = NULL, Pop = NULL, t = t,...) {
	out <- lapply(seq(N), function(x) {
			      res <- go_fish(fleet_params = fleet_params[[x]], t = t, Pop = NULL, fleet_catches = fleets[[x]])
	       })
}

