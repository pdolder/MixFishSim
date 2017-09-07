#' @title Initialise populations
#'
#' @description \code{init_pop} sets up the populations spatial distribution
#' based on the habitat preference, starting cell and 'n' numbers of movements
#' for all populations in the simulation.
#'
#' @param Bio is a Numeric vector of the starting (total) biomass for each of the
#' populations.
#' @param hab is the list of Matrices with the habitat preferences created by \code{create_hab}
#' @param start_cell is a list of Numeric vectors with the starting cells for
#' the populations
#' @param lambda is the strength that the movement distance decays at in the
#' \code{move_prob} function
#' @param init_move_steps is a Numeric indicating the number of movements to
#' initialise for the population distributions
#'
#' @return The function returns the matrices for the starting spatial
#' population densities

#' @examples init_pop(sim_init = sim_init, Bio = c(1e6, 2e5), hab = list(spp1 = matrix(nc = 10,
#' runif(10*10)), spp2 = matrix(nc = 10, runif(10*10)), lambda = c("spp1" =
#' 0.2, "spp2" = 0.3), init_move_steps = 10))
#' Note, example will not have the right biomass

#' @export

init_pop <- function(sim_init = sim_init, Bio = NULL, hab = NULL, start_cell = NULL, lambda = NULL, init_move_steps = 10) {


# set up population matrices

	# Apply over all populations, returning a list
Pop <- lapply(names(Bio), function(x) {

		      ## Initial distribution
		      PopIn <- matrix(nc = ncol(hab[[x]]), nr = nrow(hab[[x]]), 0)
		      PopIn[start_cell[1], start_cell[2]] <- Bio[[x]] 

		      # Move the population around a bit
		      # 1. Move probabilities

		      MoveProp <- move_prob_Lst(lambda = lambda[[x]], hab = hab[[x]])

		      # 2. Apply move n times

		      for (i in 1:init_move_steps) {
		      PopIn <- move_population(moveProp = MoveProp, StartPop = PopIn)
		      PopIn <- Reduce("+", PopIn)
		      }

		      # Return the starting population
		      names(PopIn) <- x
		      return(PopIn)

})


## Set up the population level recording vectors

	# extract the indices
	idx <- sim_init[["idx"]]

	# Pop level biomass
	Bio.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			  list(1:idx["ny"], 1:idx["nw"]))
	# Pop level Fs
	F.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			list(1:idx["ny"], 1:idx["nw"]))

	# Pop level catches
	Catch.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			    list(1:idx["ny"], 1:idx["nw"]))
	
# Pop level recruitment
	Rec.mat <- matrix(NA,nrow= idx["n.spp"],ncol = idx["ny"]+1,dimnames=list(paste0("spp",1:idx["n.spp"]),0:idx["ny"]))


	return(list(Pop_record = list(Bio.mat = Bio.mat, F.mat = F.mat, Catch.mat = Catch.met, Rec.mat = Rec.mat),
		    Pop = Pop))


}
