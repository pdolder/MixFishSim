#' @title Sum fleets catches
#' 
#' @description \code{sum_fleets_catches} is a helper function to apply
#' \code{sum_fleet_catches} to all fleets, returning a single list of matrices
#' with the catches of each population across all fleets and vessels.

#' @param FUN is the function, i.e. \code{sum_fleet_catches}
#' @param fleets_log is the log of all the catches for all fleets, coming from
#' application of go_fish_fleet to all fleets
#' @param n_spp is the number of populations in the simulation (NOTE: can
#' remove this and take from the overall sim settings)

#' @return is a list of matrices (one for each population) with all fleets
#' catches of each population. This is then used as an input to the baranov
#' calcs

#' @examples spp_catches <- sum_fleets_catches(FUN = sum_fleet_catches,
#' fleets_log = applied_to_fleets, n_spp = 2)

#' @export 

sum_fleets_catches <- function(FUN = sum_fleet_catches, fleets_log = NULL, sim_init = sim,...) {

	n_spp <- sim_init[["idx"]][["n.spp"]]

	## apply the function to all fleets
	out <- lapply(seq(length(fleets_log)), function(x) {
	       res <- sum_fleet_catches(fleet_log = fleets_log[[x]], sim_init = sim_init)
	       
	       })


	# Reorder the output by pop
	y <- lapply(seq(n_spp), function(x) {
	z <- lapply(out, "[[", x)
	return(z)
		    })

## Reduce to the population level
spp_catches <- lapply(y, function(x) {
	       Reduce("+", x)})

names(spp_catches) <- paste("spp", seq(n_spp), sep = "")

return(spp_catches)

}


