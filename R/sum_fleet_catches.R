#' @title Sum fleet catches
#'
#' @description \code{sum_fleet_catches} is a helper function to take the
#' spatial catches for an entire fleet and sum them as a matrix of catches for
#' the fleet for each population

#' @param sim_init is the initialised simulation settings, from \code{init_sim}
#' @param fleet_log is the output of \code{go_fish_fleet}, i.e. the catch log
#' information for a single fleet

#' @return is a list of matrices (one for each population) with the entire
#' fleets catches of the population

#' @examples test <- sum_fleet_catches(fleet_log = applied_to_fleets[[1]])

#' @export 

sum_fleet_catches <- function(sim_init = sim, fleet_log = NULL)  {

	n_spp <- sim_init[["idx"]][["n.spp"]]

y <- lapply(seq(n_spp), function(x) {
z <- lapply(fleet_log[["sp_fleets_catches"]], "[[", x)
return(z)
		    })

## Reduce to the population level
flt_catches <- lapply(y, function(x) {
	       Reduce("+", x)})

names(flt_catches) <- paste("spp", seq(n_spp), sep ="")
return(flt_catches)
}



