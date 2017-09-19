#' @title Sum fleet catches
#'
#' @description \code{sum_fleet_catches} is a helper function to take the
#' spatial catches for an entire fleet and sum them as a matrix of catches for
#' the fleet for each population

#' @param fleet_log is the output of \code{go_fish_fleet}, i.e. the catch log
#' information for a single fleet
#' @param n_spp is the number of populations in the simulation (NOTE: can
#' remove this and take from the overall sim settings)

#' @return is a list of matrices (one for each population) with the entire
#' fleets catches of the population

#' @examples test <- sum_fleet_catches(fleet_log = applied_to_fleets[[1]], n_spp = 2)

#' @export 

sum_fleet_catches <- function(fleet_log = NULL, n_spp = NULL)  {

y <- lapply(seq(n_spp), function(x) {
z <- lapply(fleet_log[["sp_fleets_catches"]], "[[", x)
return(z)
		    })

## Reduce to the population level
flt_catches <- lapply(y, function(x) {
	       Reduce("+", x)})

return(flt_catches)
}



